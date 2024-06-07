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
                #:speciality))
(in-package #:ats/models/job)


(deftable job ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor job-title)
   (description :initarg :description
                :type string
                :col-type :text
                :accessor job-description)
   (project :initarg :project
            :type project
            :col-type project
            :accessor job-project)
   (speciality :initarg :speciality
               :type (or null speciality)
               :col-type (or :null speciality)
               :accessor job-speciality)
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

