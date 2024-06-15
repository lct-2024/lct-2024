(uiop:define-package #:ats/spiders/headhunter/resume
  (:use #:cl)
  (:import-from #:scrapycl
                #:spider
                #:request)
  (:import-from #:html2text)
  (:import-from #:lquery
                #:$1
                #:$)
  (:import-from #:dexador
                #:http-request-not-found)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:serapeum
                #:dict
                #:fmt)
  (:import-from #:str
                #:trim)
  (:import-from #:ats/ai/cv
                #:make-job-profile
                #:extract-speciality
                #:extract-skills
                #:extract-programming-language)
  (:import-from #:ats/models/speciality
                #:get-speciality-id)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration+)
  (:import-from #:local-time-duration
                #:duration)
  (:import-from #:ats/models/project
                #:get-random-project-id)
  (:import-from #:ats/models/programming-language
                #:separate-skills-from-languages
                #:get-programming-language-ids)
  (:import-from #:ats/models/skill
                #:get-skill-ids)
  (:import-from #:ats/models/job-programming-language
                #:bind-job-to-programming-languages)
  (:import-from #:ats/models/job-skill
                #:bind-job-to-skills)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:quri
                #:uri-query-params))
(in-package #:ats/spiders/headhunter/resume)


(defclass resumes-page-request (request)
  ())


(defclass headhunter-jobs (spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'resumes-page-request
                                          :url "https://career.habr.com/resumes"))))


(defvar *last-response* nil)


(defvar *parsed-fio* nil)


(defun extract-page (url)
  (let ((url (quri:uri url)))
    (values
     (parse-integer
      (or (assoc-value (uri-query-params url)
                       "page"
                       :test #'string-equal)
          "1")))))


(defun process-resumes-page (response base-url)
  (let* ((current-page (extract-page base-url))
         (next-page-url
           (unless (= current-page 10)
             (fmt "https://career.habr.com/resumes?order=last_visited&page=~A"
                  (1+ current-page)))))
    (list
     ($ (initialize response)
       ".resume-card"
       (combine
        ($1 ".resume-card__title-link"
          (text))
        ($1 ".resume-card__specialization"
          (text))
        ($1 ".resume-card__offer"
          (text))
        ($ ".inline-list a"
          (filter (lambda (node)
                    (let ((href (plump:attribute node "href")))
                      (str:containsp "skills" href))))
          (text))
        ($1 ".resume-card__experience"
          (text)))
       (map-apply (lambda (fio about salary skills-raw experience)
                    (unless (member fio *parsed-fio* :test #'string-equal)
                      (multiple-value-bind (skills languages)
                          (separate-skills-from-languages (coerce skills-raw 'list))
                      
                        (let* ((parsed-data
                                 (dict "fio" (trim fio)
                                       "about" (trim about)
                                       "experience" (let ((value (trim experience)))
                                                      (if (string-equal value "• Не указан")
                                                          ""
                                                          value))
                                       "skills" skills
                                       "languages" languages
                                       "education" nil
                                       "salary" (trim salary))))
                          (with-connection ()
                            (make-job-profile parsed-data))
                          (push fio *parsed-fio*)
                          ;; Ничего не возвращаем, так как сохранили профиль в базу
                          (values)))))))
     (when next-page-url
       (make-instance 'resumes-page-request
                      :url next-page-url)))))


(defmethod scrapycl:process ((spider headhunter-jobs) (request resumes-page-request))
  (multiple-value-bind (response base-url)
      (scrapycl:fetch spider request)
    (setf *last-response* (list response base-url))
    (process-resumes-page response base-url)))


;; (defun html2text (document)
;;   "Converts given HTML string into the Markdown and returns a string as well."
;;   (let* ((html2text::*print-pretty* t)
;;          (html2text::*written-newlines* 0)
;;          (html2text::*block-index* 0))
;;     (html2text::with-output-to-string (html2text::*output-stream*)
;;       (html2text::pprint-logical-block (html2text::*output-stream* nil)
;;         (html2text::serialize nil document)))))


;; (defun normalized-text (node)
;;   (trim
;;    (regex-replace-all
;;     (fmt "~A+"
;;          #\Newline)
;;     (regex-replace-all
;;      " +"
;;      (str:replace-all
;;       (fmt "~A" #\Return)
;;       (fmt "~A" #\Newline)
;;       (html2text node))
;;      " ")
;;     (fmt "~A"
;;          #\Newline))))


;; (defun process-job-page (response job-category)
;;   ($ (initialize response)
;;     (combine
;;      ;; title
;;      ($1 ".block"
;;        (eq 0)
;;        ".block-title-text"
;;        (text))
;;      ;; Description
;;      ($1 ".block"
;;        (eq 1)
;;        ".block-subtitle"
;;        (map #'normalized-text))
;;      ;; Skills
;;      ($1 ".block"
;;        (eq 2)
;;        (map #'normalized-text)))
;;     (map-apply (lambda (title description skills)
;;                  (let* ((full-description (fmt "~A~3%~A"
;;                                                description
;;                                                skills))
;;                         (skills-as-list
;;                           (extract-skills full-description))
;;                         (speciality
;;                           (extract-speciality full-description))
;;                         (programming-language
;;                           (extract-programming-language full-description)))
;;                    (with-connection ()
;;                      (let* ((job (mito:create-dao 'ats/models/job::job
;;                                                   :title title
;;                                                   :description full-description
;;                                                   :project-id (get-random-project-id)
;;                                                   :category job-category
;;                                                   :speciality-id (get-speciality-id speciality)
;;                                                   :active t
;;                                                   :active-to (timestamp-duration+ (now)
;;                                                                                   (duration :day 30))
;;                                                   :type-of-employment "Полный рабочий день"))
;;                             (skill-ids (get-skill-ids skills-as-list))
;;                             (programming-language-ids (get-programming-language-ids
;;                                                        (list programming-language)) ))
;;                        (bind-job-to-programming-languages job
;;                                                           programming-language-ids)
;;                        (bind-job-to-skills job skill-ids)
;;                        (values))))))))


;; (defvar *items-to-skip* 0)

;; (defmethod scrapycl:process ((spider headhunter-jobs) (request job-page-request))
;;   ;; Иногда в урлах могут быть назаэнкоженные руские символы:
;;   ;; https://headhunter.com/career/vacancies/с-plus-plus-senior-developer/
;;   ;; 
;;   ;; Браузер автоматически их энкодит и урл открывается, а dexador нет и получает ошибку.
;;   ;; Пока плюём на это, и игнорируем 404 ошибки. Но надо бы сделать энкодинг вот такой:
;;   ;; https://headhunter.com/career/vacancies/%D1%81-plus-plus-senior-developer/
;;   (handler-case
;;       (multiple-value-bind (response base-url)
;;           (scrapycl:fetch spider request)
;;         (setf *last-response* (list response base-url))
;;         (cond
;;           ((zerop *items-to-skip*)
;;            (process-job-page response (job-category request)))
;;           (t
;;            (decf *items-to-skip*)
;;            (values))))
;;     (scrapycl:fetch-error ()
;;       (values))))


(defun start-scraper ()
  (scrapycl:start (make-instance 'headhunter-jobs)
                  :wait t))
