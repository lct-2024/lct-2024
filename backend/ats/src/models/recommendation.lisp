(uiop:define-package #:ats/models/recommendation
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
  (:import-from #:yason
                #:with-output-to-string*)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/models/speciality
                #:speciality))
(in-package #:ats/models/recommendation)


(deftable recommendation ()
  ((applicant :initarg :applicant
              :documentation "Пользователь, к которому привязана запись об образовании."
              :type applicant
              :col-type applicant
              :reader recommendation-applicant)
   (fio :initarg :fio
        :type string
        :col-type :text
        :initform ""
        :documentation "ФИО"
        :accessor recommendation-fio)
   (position :initarg :position
             :type string
             :col-type :text
             :initform ""
             :documentation "Должность"
             :accessor recommendation-position)
   (company :initarg :company
            :type string
            :col-type :text
            :initform ""
            :documentation "Компания"
            :accessor recommendation-company)
   (email :initarg :email
          :type string
          :col-type :text
          :initform ""
          :documentation "Email"
          :accessor recommendation-email)
   (phone :initarg :phone
          :type string
          :col-type :text
          :initform ""
          :documentation "Phone"
          :accessor recommendation-phone))
  (:table-name "ats.recommendation"))


(defmethod print-object ((recommendation recommendation) stream)
  (print-unreadable-object (recommendation stream :type t)
    (format stream "ID=~A FIO=~A"
            (object-id recommendation)
            (recommendation-fio recommendation))))

