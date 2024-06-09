(uiop:define-package #:ats/ai/cv
  (:use #:cl)
  (:import-from #:serapeum
                #:href
                #:dict
                #:fmt)
  (:import-from #:yason
                #:false
                #:with-output-to-string*)
  (:import-from #:alexandria
                #:read-file-into-string)
  (:import-from #:str
                #:replace-all)
  (:import-from #:ats/models/applicant
                #:contact
                #:applicant)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:ats/models/applicant-programming-language
                #:bind-applicant-to-programming-languages)
  (:import-from #:ats/models/programming-language
                #:get-programming-language-ids)
  (:import-from #:ats/models/skill
                #:get-skill-ids)
  (:import-from #:ats/models/education
                #:education)
  (:import-from #:ats/models/speciality
                #:get-speciality-id)
  (:import-from #:40ants-pg/connection
                #:with-connection))
(in-package #:ats/ai/cv)


(defparameter *cv-parse-prompt*
  (asdf:system-relative-pathname :ats "src/ai/prompts/cv.txt"))


(defun request-json-response (prompt input)
  (let ((token (uiop:getenv "YAGPT_KEY_TOKEN"))
        (folder-id (uiop:getenv "YAGPT_FOLDER_ID")))
    (unless token
      (error "Set YAGPT_KEY_TOKEN to a token for accessing Yandex GPT."))
    (unless folder-id
      (error "Set YAGPT_FOLDER_ID to a token for accessing Yandex GPT."))
  
    (let* ((payload (dict
                     "modelUri" (fmt "gpt://~A/yandexgpt"
                                     folder-id)
                     "completionOptions" (dict "stream" false
                                               "temperature" 0.6
                                               "maxTokens" "2000")
                     "messages" (list (dict "role" "system"
                                            "text" prompt)
                                      (dict "role" "user"
                                            "text" input))))
           (content (with-output-to-string* ()
                      (yason:encode payload)))
           (headers (list (cons "Content-Type" "application/json")
                          (cons
                           "Authorization" (fmt"Api-Key ~A"
                                               token)))))
      (let* ((response (dex:post "https://llm.api.cloud.yandex.net/foundationModels/v1/completion"
                                 :headers headers
                                 :content content
                                 :connect-timeout 30
                                 :read-timeout 30))
             (parsed (yason:parse response))
             (alternative (first (href parsed "result" "alternatives")))
             (result (when alternative
                       (href alternative
                             "message"
                             "text")))
             (parsed-result (when result
                              (yason:parse (replace-all "```" ""
                                                        (replace-all "```json" ""
                                                                     result))))))
        (values parsed-result)))))



(defun parse-cv (filename)
  (request-json-response (read-file-into-string *cv-parse-prompt*)
                         (read-file-into-string filename)))


(defun make-random-contacts (user-id)
  (append (when (< (random 100) 30)
            (list (make-instance 'contact
                                 :type "telegram"
                                 :value (fmt "user-~A" user-id))))
          (when (< (random 100) 30)
            (list (make-instance 'contact
                                 :type "github"
                                 :value (fmt "user-~A" user-id))))
          (when (< (random 100) 30)
            (list (make-instance 'contact
                                 :type "linkedin"
                                 :value (fmt "user-~A" user-id))))))

(defun parse-date (date)
  (local-time:parse-timestring
   (cond
     ((and (typep date 'string)
           (str:containsp "-" date))
      date)
     (t
      ;; Когда указан только год
      (fmt "~A-01-01" date)))))


(defun make-job-profile (parsed-data)
  (with-transaction
    (let* ((fio (href parsed-data "fio"))
           (about (href parsed-data "about"))
           (experience (href parsed-data "experience"))
           (skills (href parsed-data "skills"))
           (languages (remove-if (lambda (lang)
                                   (loop for bad-lang in '("русский" "английский" "немецкий" "французский" "китайский" "арабский")
                                         thereis (string-equal lang bad-lang)))
                                 (href parsed-data "languages")))
           ;; (desired_position (href parsed-data "desired_position"))
           (education (href parsed-data "education"))
           (user-id (+ 1000000 (random 100000)))
           (email (fmt "user-~A@example.com" user-id))
           (contacts (make-random-contacts user-id))
           (applicant (mito:create-dao 'applicant
                                       :user-id user-id
                                       :name fio
                                       :email email
                                       :experience experience
                                       :about about
                                       :contacts contacts)))
      (ats/models/applicant-skill::bind-applicant-to-skills applicant
                                                            (get-skill-ids skills))
      (bind-applicant-to-programming-languages applicant
                                               (get-programming-language-ids languages))

      ;; Добавим данные об образовании
      (loop for item in education
            for title = (href item "title")
            for speciality = (href item "speciality")
            for type = (href item "type")
            for from = (href item "from")
            for to = (href item "to")
            do (mito:create-dao 'education
                                :applicant applicant
                                :title title
                                :from (if from
                                          (parse-date from)
                                          (local-time:today))
                                :to (when to
                                      (parse-date to))
                                :type type
                                :speciality-id (when speciality
                                                 (get-speciality-id speciality))))
      (values applicant))))


(defvar *already-processed* nil)
(defvar *processed-with-error* nil)


(defun process-cv (filename)
  (unless (member filename *already-processed*
                  :test #'equalp)
    (let* ((txt-filename (merge-pathnames (make-pathname :type "txt")
                                          filename)))
      (unless (uiop:file-exists-p txt-filename)
        (uiop:run-program (list "pandoc"
                                (namestring filename)
                                "-o"
                                (namestring txt-filename))))
      
      (let ((parsed (parse-cv txt-filename)))
        (make-job-profile parsed))
      
      (push filename *already-processed*)
      (values t))))


(defun add-all-cv ()
  (loop for filename in (directory (uiop:wilden
                                    "~/CVs/"))
        when (and (pathname-type filename)
                  (not (string-equal (pathname-type filename)
                                     "txt")))
        do (handler-case
               (with-connection ()
                 (process-cv filename))
             (serious-condition ()
               (push filename *processed-with-error*)))))


(defun extract-skills (text)
  (gethash
   "results"
   (request-json-response
    "Выдели из этого текста IT технологии. Сократи каждую технологию до двух-трёх слов. Представь результат в виде JSON документа, где есть ключ \"results\" и извлечённые данные в как список из строк."
    text)))


(defun extract-programming-language (text)
  (gethash
   "results"
   (request-json-response
    "Выдели из этого текста основной язык программирования. Представь результат в виде JSON документа, где есть ключ \"results\" и извлечённые данные в как строка. Сократи результат до 2-3 слов."
    text)))

(defun extract-speciality (text)
  (gethash
   "results"
   (request-json-response
    "Выдели из этого специальность кандидата, которого ищут на работу. Представь результат в виде JSON документа, где есть ключ \"results\" и извлечённые данные в как строка. Сократи результат до 2-3 слов."
    text)))
