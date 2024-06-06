(uiop:define-package #:ats/models/applicant
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
(in-package #:ats/models/applicant)


(deftable applicant ()
  ((user-id :initform :user-id
            :documentation "ID пользователя из микросервиса Passport."
            :type integer
            :col-type :bigint
            :reader applicant-user-id)
   (name :initarg :name
         :type string
         :col-type :text
         :accessor applicant-name)
   (email :initarg :email
          :type string
          :col-type :text
          :accessor applicant-email))
  (:table-name "ats.applicant"))


(defmethod print-object ((applicant applicant) stream)
  (print-unreadable-object (applicant stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id applicant)
            (applicant-name applicant))))

