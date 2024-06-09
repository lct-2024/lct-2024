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
                #:programming-language)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:export
   #:bind-job-to-programming-languages
   #:get-job-programming-languages))
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


(defun bind-job-to-programming-languages (job programming-language-ids)
  (with-transaction
      (loop for programming-language-id in programming-language-ids
            for link = (mito:create-dao 'job-programming-language
                                        :job job
                                        :programming-language-id programming-language-id)
            collect (programming-language link))))


(defun get-job-programming-languages (job)
  (mito:select-by-sql 'programming-language
                      "
select programming_language.*
from ats.programming_language
join ats.job_programming_language on programming_language.id = job_programming_language.programming_language_id
where job_programming_language.job_id = ?"
                      :binds (list (mito:object-id job))))
