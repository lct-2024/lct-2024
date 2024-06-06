(uiop:define-package #:ats/models/job
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
                #:sql-fetch-all))
(in-package #:ats/models/job)


(deftable job ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor job-title)
   (description :initarg :description
                :type string
                :col-type :text
                :accessor job-description))
  (:table-name "ats.job"))


(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id job)
            (job-title job))))

