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
                #:with-output-to-string*)
  (:import-from #:common/chat
                #:create-new-chat)
  (:import-from #:40ants-pg/connection
                #:with-connection))
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
   (portfolio :initarg :portfolio
          :type string
          :col-type :text
          :initform ""
          :documentation "Ссылка на портфолио."
          :accessor applicant-portfolio)
   (salary :initarg :salary
          :type string
          :col-type :text
          :initform ""
          :documentation "Желаемая зарплата"
          :accessor applicant-salary)
   (contacts :initarg :contacts
             :type (soft-list-of contact)
             :col-type :jsonb
             :initform nil
             :inflate #'contacts-from-json
             :deflate #'contacts-to-json
             :documentation "Список контактов в виде словарей с ключами \"type\" и \"value\", где значениями являются строки. Например: [{\"telegram\": \"telegram-nick\"}]."
             :accessor applicant-contacts)
   (chat-id :initarg :chat-id
            :initform nil
            :type (or null string)
            :col-type (or :null :text)
            :documentation "ID чата, привязанного к объекту."
            :accessor applicant-chat-id)
   (system-chat-id :initarg :system-chat-id
                   :initform nil
                   :type (or null string)
                   :col-type (or :null :text)
                   :documentation "ID чата, с системными сообщениями пользователя."
                   :accessor applicant-system-chat-id))
  (:table-name "ats.applicant"))


(defmethod print-object ((applicant applicant) stream)
  (print-unreadable-object (applicant stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id applicant)
            (applicant-name applicant))))



(defun fill-chat-ids ()
  (with-connection ()
    (loop for applicant in (mito:select-dao 'applicant)
          do (setf (applicant-chat-id applicant)
                   (create-new-chat "applicant"
                                    (princ-to-string (mito:object-id applicant))))
             (mito:save-dao applicant))))


(defun fill-system-chat-ids ()
  (with-connection ()
    (loop for applicant in (mito:select-dao 'applicant)
          do (setf (applicant-system-chat-id applicant)
                   (create-new-chat "user-notifications"
                                    (princ-to-string (applicant-user-id applicant))))
             (mito:save-dao applicant))))


(defmethod mito:insert-dao :after ((obj applicant))
  (setf (applicant-chat-id obj)
        (create-new-chat "applicant"
                         (princ-to-string (mito:object-id obj))))
  (setf (applicant-system-chat-id obj)
        (create-new-chat "user-notifications"
                         (princ-to-string (applicant-user-id obj))))
  (mito:save-dao obj))
