(uiop:define-package #:passport/models/role
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:passport/token
                #:issue-token)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:common/utils
                #:encode-json
                #:decode-json))
(in-package #:passport/models/role)


(deftable role ()
  ((name :initarg :name
         :type string
         :col-type :text
         :accessor role-name))
  (:table-name "passport.role"))


(defmethod print-object ((role role) stream)
  (print-unreadable-object (role stream :type t)
    (format stream "ID=~A NAME=~S"
            (object-id role)
            (role-name role))))
