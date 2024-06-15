(uiop:define-package #:passport/models/user
  (:use #:cl)
  (:import-from #:serapeum
                #:->
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
(in-package #:passport/models/user)


(deftable user ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (fio :initarg :fio
        :type string
        :col-type :text
        :accessor user-fio)
   (email :initarg :email
          :type string
          :col-type (or :null :text)
          :accessor user-email)
   ;; TODO: не отдавать поле через API
   (password-hash :initarg :password-hash
                  :type string
                  :col-type (or :null :text)
                  :reader user-password-hash)
   (avatar-url :initarg :avatar-url
               :type string
               :col-type (or :null :text)
               :accessor avatar-url)
   (banned :col-type :boolean
           :type boolean
           :initform nil
           :reader user-banned-p
           :documentation "Если True, то пользователь забанен и не может логиниться.")
   (metadata :col-type :jsonb
             :type hash-table
             :initform (dict)
             :reader user-metadata
             :inflate #'decode-json
             :deflate #'encode-json
             :documentation "Словарь с дополнительной информацией о пользователе."))
  (:table-name "passport.user"))


(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "ID=~A EMAIL=~S FIO=~S"
            (object-id user)
            (user-email user)
            (user-fio user))))


(defun get-user-by (email)
  (mito:find-dao 'user :email email))


(-> get-user-roles-and-scopes (user)
    (values (soft-list-of string)
            (soft-list-of string)
            &optional))

(defun get-user-roles-and-scopes (user)
  (loop for row in (mito:retrieve-by-sql "
select r.name
     , array_agg(s.name) as scopes
  from passport.user_role as ur
  join passport.role as r on ur.role_id = r.id
  join passport.role_scope as rs on r.id = rs.role_id
  join passport.scope as s on rs.scope_id = s.id
 where ur.user_id = ?
 group by r.name
 order by r.name
"
                                         :binds (list (object-id user)))
        for role-name = (getf row :name)
        for scopes = (coerce (getf row :scopes)
                             'list)
        collect role-name into all-roles
        append scopes into all-scopes
        finally (return (values all-roles
                                (remove-duplicates all-scopes
                                                   :test #'string=)))))


(defun issue-token-for (user)
  (multiple-value-bind (roles scopes)
      (get-user-roles-and-scopes user)
    (let ((payload (dict "user-id" (object-id user)
                         "fio" (user-fio user)
                         "roles" roles
                         "scopes" scopes)))
      (issue-token payload))))


;; Не помню зачем я так странно выбирал id пользователя, когда там автоинкремент прекрасно справляется
;; А такой способ вызывает проблемы при параллельном выполнении запросов:
;; (defun get-next-user-id ()
;;   (let* ((rows (sql-fetch-all "select coalesce(max(id), 0) + 1 as next_id from passport.user")))
;;     (getf (first rows) :|next_id|)))


(defun is-email-available-p (email)
  (let* ((rows (sql-fetch-all "select 1 as value from passport.user where email = ?" email)))
    (null rows)))


(defun ban-user (object)
  (setf (slot-value object 'banned)
        t)
  (mito:save-dao object))


(defun unban-user (object)
  (setf (slot-value object 'banned)
        nil)
  (mito:save-dao object))
