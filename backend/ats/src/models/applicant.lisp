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
                #:sql-fetch-all)
  (:import-from #:yason
                #:with-output-to-string*))
(in-package #:ats/models/applicant)


(defclass contact ()
  ((type :initarg :type
         :type string
         :accessor contact-type)
   (value :initarg :value
          :type string
          :accessor contact-value)))


(deftype contacts ()
  '(soft-list-of contact))


(defun contacts-to-json (contacts)
  (with-output-to-string* ()
    (yason:encode
     (loop for contact in contacts
           collect (dict "type" (contact-type contact)
                         "value" (contact-type contact))))))

(defun contacts-from-json (string)
  (let ((items (yason:parse string)))
    (loop for item in items
          collect
             (make-instance 'contact
                            :type (gethash "type" item "unknown")
                            :value (gethash "value" item "unknown")))))


(deftable applicant ()
  ((user-id :initarg :user-id
            :documentation "ID пользователя из микросервиса Passport."
            :type integer
            :col-type :bigint
            :reader applicant-user-id)
   (name :initarg :name
         :type string
         :col-type :text
         :initform ""
         :accessor applicant-name)
   (email :initarg :email
          :type string
          :col-type :text
          :initform ""
          :accessor applicant-email)
   (experience :initarg :experience
               :type string
               :col-type :text
               :initform ""
               :documentation "Описание опыта работы кандидата."
               :accessor applicant-experience)
   (about :initarg :about
          :type string
          :col-type :text
          :initform ""
          :documentation "Общее описание кандидата, его увелечения, свойства характера и прочее."
          :accessor applicant-about)
   (contacts :initarg :contacts
             :type (soft-list-of contact)
             :col-type :jsonb
             :initform nil
             :inflate #'contacts-from-json
             :deflate #'contacts-to-json
             :documentation "Список контактов в виде словарей с ключами \"type\" и \"value\", где значениями являются строки. Например: [{\"telegram\": \"telegram-nick\"}]."
             :accessor applicant-contacts))
  (:table-name "ats.applicant"))


(defmethod print-object ((applicant applicant) stream)
  (print-unreadable-object (applicant stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id applicant)
            (applicant-name applicant))))

