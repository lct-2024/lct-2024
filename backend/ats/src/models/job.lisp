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
                #:sql-fetch-all)
  (:import-from #:ats/models/project
                #:project)
  (:import-from #:ats/models/speciality
                #:speciality)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:timestamp-duration+
                #:duration)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:common/chat
                #:create-new-chat))
(in-package #:ats/models/job)


(deftable job ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor job-title)
   (category :initarg :category
             :type string
             :col-type :text
             :initform "Другое"
             :documentation "Одна из нескольких категорий к которой можно отнести вакансию: Разработка, Аналитика, Тестирование и тд."
             :accessor job-category)
   (city :initarg :city
         :type string
         :col-type :text
         :initform "Москва"
         :documentation "Город, в котором идёт набор кандидатов."
         :accessor job-city)
   (description :initarg :description
                :type string
                :col-type :text
                :accessor job-description)
   (project :initarg :project
            :type project
            :col-type project
            :documentation "Проект к которому привязана джоба."
            :accessor job-project)
   (speciality :initarg :speciality
               :type (or null speciality)
               :col-type (or :null speciality)
               :accessor job-speciality)
   (active :initarg :active
           :type boolean
           :col-type :boolean
           :initform t
           :documentation "Признак того, что идёт набор на эту вакансию."
           :accessor job-active)
   (active-to :initarg :active-to
              :type (or null local-time:timestamp)
              :col-type (or :null :timestamptz)
              :initform (timestamp-duration+ (now)
                                             (duration :day 30))
              :documentation "Признак того, что идёт набор на эту вакансию."
              :accessor job-active-to)
   (type-of-employment :initarg :type-of-employment
                       :initform "Полная"
                       :type string
                       :col-type :text
                       :documentation "Вид занятости, например: \"Полная\", \"Частичная\", \"Стажировка\"."
                       :accessor job-type-of-employment)
   (salary :initarg :salary
           :initform nil
           :type (or null string)
           :col-type (or :null :text)
           :documentation "Примерный оклад, как цифра или диапазон."
           :accessor job-salary)
   (required-experience :initarg :required-experience
           :initform nil
           :type (or null string)
           :col-type (or :null :text)
           :documentation "Опыт работы, лет или месяцев."
           :accessor job-required-experience)
   (chat-id :initarg :chat-id
           :initform nil
           :type (or null string)
           :col-type (or :null :text)
           :documentation "ID чата, привязанного к объекту."
           :accessor job-chat-id))
  (:table-name "ats.job"))


(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id job)
            (job-title job))))



(defun fill-chat-ids ()
  (with-connection ()
    (loop for job in (mito:select-dao 'job)
          do (setf (job-chat-id job)
                   (create-new-chat "job"
                                    (princ-to-string (mito:object-id job))))
             (mito:save-dao job))))


(defmethod mito:insert-dao :after ((obj job))
  (setf (job-chat-id obj)
        (create-new-chat "job"
                         (princ-to-string (mito:object-id obj))))
  (mito:save-dao obj))
