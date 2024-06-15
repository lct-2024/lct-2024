(uiop:define-package #:passport/models/user-role
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
  (:import-from #:passport/models/user
                #:user)
  (:import-from #:passport/models/role
                #:role))
(in-package #:passport/models/user-role)


(deftable user-role ()
  ((user :initarg :user
         :type user
         :col-type user
         :accessor bound-user)
   (role :initarg :role
         :type role
         :col-type role
         :accessor bound-role))
  (:table-name "passport.user_role"))


(defmethod print-object ((user-role user-role) stream)
  (print-unreadable-object (user-role stream :type t)
    (format stream "ID=~A USER=~S ROLE=~S"
            (object-id user-role)
            (bound-user user-role)
            (bound-role user-role))))
