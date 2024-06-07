(uiop:define-package #:ats/models/job-programming-language
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:ats/models/job
                #:job)
  (:import-from #:ats/models/programming-language
                #:programming-language))
(in-package #:ats/models/job-programming-language)


(deftable job-programming-language ()
  ((job :initarg :job
            :type job
            :col-type job
            :accessor job)
   (programming-language :initarg :programming-language
                         :type programming-language
                         :col-type programming-language
                         :accessor programming-language))
  (:table-name "ats.job_programming_language"))


(defmethod print-object ((obj job-programming-language) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A JOB=~A LANG=~A"
            (object-id obj)
            (job obj)
            (programming-language obj))))

