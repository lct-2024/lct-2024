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
                #:now-duration
                #:timestamp-duration+)
  (:import-from #:local-time-duration
                #:duration))
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
                       :accessor job-type-of-employment))
  (:table-name "ats.job"))


(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id job)
            (job-title job))))

