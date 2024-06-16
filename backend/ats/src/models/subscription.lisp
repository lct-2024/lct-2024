(uiop:define-package #:ats/models/subscription
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:retrieve-by-sql
                #:create-dao
                #:select-dao
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:ats/models/job
                #:job)
  (:import-from #:ats/models/applicant
                #:applicant-email
                #:applicant)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:ats/models/application-step
                #:application-step)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:alexandria
                #:random-elt)
  (:import-from #:common/utils
                #:decode-json
                #:encode-json)
  (:export
   #:bind-job-to-applicants
   #:get-subscriptions))
(in-package #:ats/models/subscription)


(deftable subscription ()
  ((applicant :initarg :applicant
              :type applicant
              :col-type applicant)
   (filters :initarg :filters
            :type hash-table
            :col-type :jsonb
            :inflate #'decode-json
            :deflate #'encode-json)
   (last-processed-job-id :initarg :last-processed-job-id
                          :type integer
                          :col-type :integer
                          :accessor last-processed-job-id))
  (:documentation "Хранит информацию о подписках пользователей на новые вакансии.")
  (:table-name "ats.subscriptions"))


(defmethod print-object ((obj subscription) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "JOB=~A FILTER=~A LAST-PROCESSED-ID=~A"
            (subscription-applicant obj)
            (subscription-filters obj)
            (last-processed-job-id obj))))


(defun add-subscription (applicant &key speciality city project category)
  (let ((filters (dict))
        (max-job-id
          (or (getf (first (retrieve-by-sql "
select max(id) as id from ats.job"))
                    :id)
              0)))
    
    (when speciality
      (setf (gethash "speciality" filters)
            speciality))
    (when city
      (setf (gethash "city" filters)
            city))
    (when project
      (setf (gethash "project" filters)
            project))
    (when category
      (setf (gethash "category" filters)
            category))
    
    (create-dao 'subscription
                :applicant applicant
                :filters filters
                :last-processed-job-id max-job-id)))
