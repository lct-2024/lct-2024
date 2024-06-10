(uiop:define-package #:passport/models/role-scope
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
                #:decode-json)
  (:import-from #:passport/models/role
                #:role)
  (:import-from #:passport/models/scope
                #:scope))
(in-package #:passport/models/role-scope)


(deftable role-scope ()
  ((role :initarg :role
         :type role
         :col-type role
         :accessor bound-role)
   (scope :initarg :scope
          :type scope
         :col-type scope
         :accessor bound-scope))
  (:table-name "passport.role_scope"))


(defmethod print-object ((role-scope role-scope) stream)
  (print-unreadable-object (role-scope stream :type t)
    (format stream "ID=~A ROLE=~S SCOPE=~S"
            (object-id role-scope)
            (bound-role role-scope)
            (bound-scope role-scope))))
