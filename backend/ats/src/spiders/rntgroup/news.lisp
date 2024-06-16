(uiop:define-package #:ats/spiders/rntgroup/news
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
(in-package #:ats/spiders/rntgroup/news)


(defclass news-page-request (request)
  ())

(defclass post-page-request (request)
  ())


(defclass rntgroup-news (spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'news-page-request
                                          :url "https://www.rntgroup.com/media/news/"))))


(defvar *last-response* nil)


(defun process-news-page (response base-url)
  ($ (initialize response)
    ".article-wrap"
    (attr "href")
    (merge-url-with base-url)
    ;; (combine
    ;;  ($1 "div"
    ;;    (text)
    ;;    (map #'str:trim))

    ;;  ($1
    ;;    (parents ".tariff")
    ;;    ".tariff-btn_wrapper a"
    ;;    (attr "href")
    ;;    (merge-url-with base-url)
    ;;    ))
    (map (lambda (url)
           (make-instance 'post-page-request
                          :url url)))))


(defmethod scrapycl:process ((spider rntgroup-news) (request news-page-request))
  (multiple-value-bind (response base-url)
      (scrapycl:fetch spider request)
    (setf *last-response* (list response base-url))
    (process-news-page response base-url)))


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

(defun background-url (text)
  "Extracts url from a text like this:

   background-image: url(/upload/resize_cache/iblock/9a1/1920_850_1/0qni7sfsiojfujoyuhabod8n2wf0g454.jpg);

"
  (cl-ppcre:register-groups-bind (url)
      ("url\\((.*)\\)" text)
    url))


(defun process-post-page (response base-url)
  ($ (initialize response)
    (combine
     ($1 ".block-title-text"
       (text)
       (map #'trim))
     ($1 ".block-desc"
       (map #'normalized-text))
     ($1 ".block1-2-bg-image"
       (attr "style")
       (map #'background-url)
       (merge-url-with base-url)))
    (map-apply (lambda (title text image-url)
                 (with-connection ()
                   (mito:create-dao 'ats/models/news-post::news-post
                                    :title title
                                    :text text
                                    :image image-url))))))


(defmethod scrapycl:process ((spider rntgroup-news) (request post-page-request))
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
        (process-post-page response base-url)
        (values))
    (scrapycl:fetch-error ()
      (values))))


(defun start-scraper ()
  (scrapycl:start (make-instance 'rntgroup-news)
                  :wait t))
