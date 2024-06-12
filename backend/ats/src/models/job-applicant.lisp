(uiop:define-package #:ats/models/job-applicant
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
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:ats/models/application-step
                #:application-step)
  (:export
   #:bind-job-to-applicants
   #:get-job-applicants))
(in-package #:ats/models/job-applicant)


(deftable job-applicant ()
  ((job :initarg :job
        :type job
        :col-type job
        :accessor job)
   (type :initarg :type
         :type string
         :col-type :text
         :documentation "Type of the link: self-applied, recommended, parsed"
         :accessor application-type)
   (status :initarg :status
           :type (or null string)
           :col-type (or :null :text)
           :documentation "Статус отклика: нанят, отказ. Статус ставится только на финальном этапе обработки кандидата."
           :accessor application-status)
   (applicant :initarg :applicant
              :type applicant
              :col-type applicant
              :accessor applicant)
   (application-step :initarg :application-step
                     :type (or null application-step)
                     :col-type (or :null application-step)
                     :accessor application-step))
  (:table-name "ats.job_applicant"))


(defmethod print-object ((obj job-applicant) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A JOB=~A APPLICANT=~A"
            (object-id obj)
            (job obj)
            (applicant obj))))


(defun apply-to-the-job (job applicant-id &key (type "parsed"))
  (or (mito:find-dao 'job-applicant
                     :job job
                     :applicant-id applicant-id)
      (mito:create-dao 'job-applicant
                       :job job
                       :type type
                       :applicant-id applicant-id)))


(defun bind-job-to-applicants (job applicant-ids &key (type "parsed"))
  (with-transaction
    (loop for applicant-id in applicant-ids
          for link = (apply-to-the-job job applicant-id :type type)
          collect (applicant link))))


(defun get-job-applicants (job)
  (mito:select-by-sql 'applicant
                      "
select applicant.*
from ats.applicant
join ats.job_applicant on applicant.id = job_applicant.applicant_id
where job_applicant.job_id = ?
order by applicant.name collate \"ru_RU\""
                      :binds (list (mito:object-id job))))
