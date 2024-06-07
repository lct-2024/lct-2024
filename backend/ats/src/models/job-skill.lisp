(uiop:define-package #:ats/models/job-skill
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
  (:import-from #:ats/models/skill
                #:skill))
(in-package #:ats/models/job-skill)


(deftable job-skill ()
  ((job :initarg :job
            :type job
            :col-type job
            :accessor job)
   (skill :initarg :skill
                         :type skill
                         :col-type skill
                         :accessor skill))
  (:table-name "ats.job_skill"))


(defmethod print-object ((obj job-skill) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A JOB=~A LANG=~A"
            (object-id obj)
            (job obj)
            (skill obj))))

