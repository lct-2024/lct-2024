(uiop:define-package #:ats/spiders/rntgroup/jobs
  (:use #:cl)
  (:import-from #:scrapycl
                #:spider
                #:request)
  (:import-from #:lquery
                #:$1
                #:$)
  (:import-from #:dexador
                #:http-request-not-found)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:str
                #:trim)
  (:import-from #:ats/ai/cv
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
                #:get-programming-language-ids)
  (:import-from #:ats/models/skill
                #:get-skill-ids)
  (:import-from #:ats/models/job-programming-language
                #:bind-job-to-programming-languages)
  (:import-from #:ats/models/job-skill
                #:bind-job-to-skills)
  (:import-from #:40ants-pg/connection
                #:with-connection))
(in-package #:ats/spiders/rntgroup/jobs)


(defclass jobs-page-request (request)
  ())


(defclass job-page-request (request)
  ((category :initarg :category
             :reader job-category)))


(defclass rntgroup-jobs (spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'jobs-page-request
                                          :url "https://rntgroup.com/career/vacancies/"))))


(defvar *last-response* nil)


(defun process-jobs-page (response base-url)
  ($ (initialize response)
    ".tariff-stickers"
    (combine
     ($1 "div"
       (text)
       (map #'str:trim))

     ($1
       (parents ".tariff")
       ".tariff-btn_wrapper a"
       (attr "href")
       (merge-url-with base-url)
       ))
    (map-apply (lambda (category url)
                 (make-instance 'job-page-request
                                :category category
                                :url url)))))


(defmethod scrapycl:process ((spider rntgroup-jobs) (request jobs-page-request))
  (multiple-value-bind (response base-url)
      (scrapycl:fetch spider request)
    (setf *last-response* (list response base-url))
    (process-jobs-page response base-url)))


(defun html2text (document)
  "Converts given HTML string into the Markdown and returns a string as well."
  (let* ((html2text::*print-pretty* t)
         (html2text::*written-newlines* 0)
         (html2text::*block-index* 0))
    (html2text::with-output-to-string (html2text::*output-stream*)
      (html2text::pprint-logical-block (html2text::*output-stream* nil)
        (html2text::serialize nil document)))))


(defun normalized-text (node)
  (trim
   (regex-replace-all
    (fmt "~A+"
         #\Newline)
    (regex-replace-all
     " +"
     (str:replace-all
      (fmt "~A" #\Return)
      (fmt "~A" #\Newline)
      (html2text node))
     " ")
    (fmt "~A"
         #\Newline))))


(defun process-job-page (response job-category)
  ($ (initialize response)
    (combine
     ;; title
     ($1 ".block"
       (eq 0)
       ".block-title-text"
       (text))
     ;; Description
     ($1 ".block"
       (eq 1)
       ".block-subtitle"
       (map #'normalized-text))
     ;; Skills
     ($1 ".block"
       (eq 2)
       (map #'normalized-text)))
    (map-apply (lambda (title description skills)
                 (let* ((full-description (fmt "~A~3%~A"
                                               description
                                               skills))
                        (skills-as-list
                          (extract-skills full-description))
                        (speciality
                          (extract-speciality full-description))
                        (programming-language
                          (extract-programming-language full-description)))
                   (with-connection ()
                     (let* ((job (mito:create-dao 'ats/models/job::job
                                                  :title title
                                                  :description full-description
                                                  :project-id (get-random-project-id)
                                                  :category job-category
                                                  :speciality-id (get-speciality-id speciality)
                                                  :active t
                                                  :active-to (timestamp-duration+ (now)
                                                                                  (duration :day 30))
                                                  :type-of-employment "Полный рабочий день"))
                            (skill-ids (get-skill-ids skills-as-list))
                            (programming-language-ids (get-programming-language-ids
                                                       (list programming-language)) ))
                       (bind-job-to-programming-languages job
                                                          programming-language-ids)
                       (bind-job-to-skills job skill-ids)
                       (values))))))))


(defvar *items-to-skip* (- 21 5))

(defmethod scrapycl:process ((spider rntgroup-jobs) (request job-page-request))
  ;; Иногда в урлах могут быть назаэнкоженные руские символы:
  ;; https://rntgroup.com/career/vacancies/с-plus-plus-senior-developer/
  ;; 
  ;; Браузер автоматически их энкодит и урл открывается, а dexador нет и получает ошибку.
  ;; Пока плюём на это, и игнорируем 404 ошибки. Но надо бы сделать энкодинг вот такой:
  ;; https://rntgroup.com/career/vacancies/%D1%81-plus-plus-senior-developer/
  (handler-case
      (multiple-value-bind (response base-url)
          (scrapycl:fetch spider request)
        (setf *last-response* (list response base-url))
        (cond
          ((zerop *items-to-skip*)
           (process-job-page response (job-category request)))
          (t
           (decf *items-to-skip*)
           (values))))
    (scrapycl:fetch-error ()
      (values))))
