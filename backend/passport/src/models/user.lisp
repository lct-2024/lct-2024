(uiop:define-package #:passport/models/user
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
   (admin :initarg :admin
          :initform nil
          :type boolean
          :col-type :boolean
          :accessor adminp
          :documentation "Если этот признак True, то пользователь считается админом и может пользоватся интерфейсом для модерации.")
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


(defun issue-token-for (user)
  (let ((payload (dict "user-id" (object-id user)
                       "fio" (user-fio user)
                       ;; Пока у нас только одна роль. Но на будущее, роли отдаются списоком:
                       "roles" (when (adminp user)
                                 (list "admin")))))
    (issue-token payload)))


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
