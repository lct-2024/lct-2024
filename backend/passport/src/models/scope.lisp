(uiop:define-package #:passport/models/scope
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
(in-package #:passport/models/scope)


(deftable scope ()
  ((name :initarg :name
         :type string
         :col-type :text
         :accessor scope-name)
   (description :initarg :description
                :type string
                :col-type :text
                :accessor scope-description))
  (:documentation "Обозначает некую бизнес-область, на которую могут быть права у пользователя. Например name может быть: blog.writer или admin, и тд.. Scopes привязаны к ролям, а роли - к пользователям.")
  (:table-name "passport.scope"))


(defmethod print-object ((scope scope) stream)
  (print-unreadable-object (scope stream :type t)
    (format stream "ID=~A NAME=~S"
            (object-id scope)
            (scope-name scope))))
