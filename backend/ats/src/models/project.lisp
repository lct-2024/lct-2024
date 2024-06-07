(uiop:define-package #:ats/models/project
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
(in-package #:ats/models/project)


(deftable project ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor project-title)
   (description :initarg :description
                :type string
                :col-type :text
                :accessor project-description))
  (:table-name "ats.project"))


(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id project)
            (project-title project))))

