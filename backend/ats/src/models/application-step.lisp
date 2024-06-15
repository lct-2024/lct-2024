(uiop:define-package #:ats/models/application-step
  (:use #:cl)
  (:import-from #:dex)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:create-dao
                #:insert-dao
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:str
                #:split
                #:trim))
(in-package #:ats/models/application-step)


(deftable application-step ()
  ((num :initarg :num
        :type integer
        :col-type :integer
        :documentation "Номер этапа в воронке найма."
        :accessor application-step-num)
   (title :initarg :title
          :type string
          :col-type :text
          :accessor application-step-title))
  (:table-name "ats.application_step"))


(defmethod print-object ((application-step application-step) stream)
  (print-unreadable-object (application-step stream :type t)
    (format stream "ID=~A NUM=~A TITLE=~A"
            (object-id application-step)
            (application-step-num application-step)
            (application-step-title application-step))))



