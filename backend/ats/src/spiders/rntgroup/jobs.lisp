(uiop:define-package #:ats/spiders/rntgroup/jobs
  (:use #:cl)
  (:import-from #:scrapycl
                #:spider
                #:request)
  (:import-from #:lquery
                #:$1
                #:$))
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
       (merge-url-with base-url)))
    (map-apply (lambda (category url)
                 (make-instance 'job-page-request
                                :category category
                                :url url)))))


(defmethod scrapycl:process ((spider rntgroup-jobs) (request jobs-page-request))
  (multiple-value-bind (response base-url)
      (scrapycl:fetch spider request)
    (setf *last-response* (list response base-url))
    (process-jobs-page response base-url)))
