(uiop:define-package #:ats/models/education
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
(in-package #:ats/models/education)


(deftable education ()
  ((applicant :initarg :applicant
              :documentation "Пользователь, к которому привязана запись об образовании."
              :type applicant
              :col-type applicant
              :reader education-applicant)
   (title :initarg :title
          :type string
          :col-type :text
          :initform ""
          :documentation "Название образовательного учреждения"
          :accessor education-title)
   (speciality :initarg :speciality
               :documentation "Полученная специальность"
               :type (or null speciality)
               :col-type (or :null speciality)
               :reader education-speciality)
   (type :initarg :type
         :type string
         :col-type :text
         :initform ""
         :documentation "Тип образования: высшее, среднее, незаконенное высше, курс, и тд."
         :accessor education-type)
   (from :initarg :from
         :type local-time:date
         :col-type :date
         :initform ""
         :documentation "Дата начала обучения"
         :accessor education-from)
   (to :initarg :to
       :type (or null local-time:date)
       :col-type (or :null :date)
       :initform nil
       :documentation "Дата окончания обучения или none, если ещё учится."
       :accessor education-to))
  (:table-name "ats.education"))


(defmethod print-object ((education education) stream)
  (print-unreadable-object (education stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id education)
            (education-title education))))

