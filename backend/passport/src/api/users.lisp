(uiop:define-package #:passport/api/users
  (:use #:cl)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:openrpc-server
                #:type-to-schema
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:mito
                #:deftable
                #:save-dao
                #:find-dao)
  (:import-from #:passport/models/user
                #:get-user-roles-and-scopes
                #:user-password-hash
                #:get-user-by
                #:issue-token-for
                ;; #:get-next-user-id
                #:is-email-available-p
                #:user)
  (:import-from #:passport/avatar
                #:get-avatar-url-for)
  (:import-from #:passport/api
                #:passport-api)
  (:import-from #:flexi-streams
                #:string-to-octets)
  (:import-from #:alexandria
                #:with-output-to-file
                #:make-keyword)
  (:import-from #:serapeum
                #:soft-list-of
                #:dict
                #:fmt)
  (:import-from #:local-time
                #:now
                #:format-rfc3339-timestring))
(in-package #:passport/api/users)


(defvar *demo-mode*
  (uiop:getenv "DEMO_MODE")
  "Если установлен демо-режим, то можно войти в любую учётку с любым паролем.")


(defun auth-log (message &rest arguments)
  (with-output-to-file (s "/home/art/lct24-auth.log"
                          :if-exists :append
                          :if-does-not-exist :create)
    (format s "[~A]: ~A~%"
            (format-rfc3339-timestring nil (now))
            (apply #'serapeum:fmt message arguments ))))


(define-rpc-method (passport-api login) (email password)
  (:summary "Позволяет залогиниться пользователю по email и паролю.")
  (:param email string)
  (:param password string)
  (:result string)
  (with-connection ()
    (let* ((hash (get-password-hash password))
           (user (get-user-by email))
           (user-hash (when user
                        (user-password-hash user))))
      (cond
        ((and user
              (or *demo-mode*
                  (equal user-hash hash)))
         (auth-log "User logged in: ~A" email)
         (issue-token-for user))
        (t
         (auth-log "User entered wrong pass or missing in db: ~A" email)
         (openrpc-server:return-error "Неправильный email или пароль." :code 1))))))


(define-rpc-method (passport-api signup) (email password
                                                &key
                                                fio ;; Это надо бы совсем выпилить
                                                metadata)
  (:summary "Регистрирует новую учётку с указанным email и паролем.")
  (:param email string "Email пользователя.")
  (:param password string "Пароль.")
  (:param fio string "Deprecated. Класть ФИО надо в metadata, как отдельные поля.")
  (:param metadata hash-table "Словарь с дополнительной информацией о пользователе, нужной остальным сервисам сайта.")
  
  (:result string)
  (log:info "Signup with" email)

  (with-connection ()
    (cond
      ((is-email-available-p email)
       (auth-log "User tried to sign up: ~A" email)
       (let* ((user (mito:create-dao 'user
                                     :fio fio
                                     :email email
                                     :avatar-url (get-avatar-url-for email)
                                     :password-hash (get-password-hash password)
                                     :metadata (or metadata
                                                   (dict)))))
         (auth-log "User signed up: ~A" email)
         (issue-token-for user)))
      (t
       (auth-log "User used email which already taken: ~A" email)
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(deftable user-profile (user)
  ((roles :initarg :roles
          :type (soft-list-of string)
          :col-type :array
          :reader user-roles)
   (scopes :initarg :scopes
          :type (soft-list-of string)
          :col-type :array
          :reader user-scopes))
  (:table "passport.user"))


(defmethod openrpc-server:slots-to-exclude ((obj (eql (find-class 'user-profile))))
  (list* "password-hash"
         (call-next-method)))


(define-rpc-method (passport-api my-profile) ()
  (:summary "Отдаёт профиль текущего залогиненого пользователя.")
  (:result user)
  (with-connection ()
    (with-session (user-id)
      (let ((user (mito:find-dao 'user
                                 :id user-id)))
        (multiple-value-bind (roles scopes)
            (get-user-roles-and-scopes user)
          (change-class user 'user-profile
                        :roles roles
                        :scopes scopes)
          (values user))))))


(define-rpc-method (passport-api get-user-profiles) (user-ids)
  (:summary "Отдаёт профили пользователей по их id.")
  (:param user-ids (soft-list-of integer) "Список id пользователей.")
  (:result (soft-list-of user))
  (let ((unique-user-ids  (remove-duplicates user-ids)))
    (when unique-user-ids
          (with-connection ()
      
            (loop for user in (40ants-pg/utils:select-dao-by-ids 'user user-ids)
                  ;; TODO: так для каждого пользователя получать роли не оптимально,
                  ;; но на этапе MVP нам это не важно. Сами роли нам нужны
                  ;; для того, чтобы можно было в чатах подсветить сотрудников компании
                  collect (multiple-value-bind (roles scopes)
                              (get-user-roles-and-scopes user)
                            (change-class user 'user-profile
                                          :roles roles
                                          :scopes scopes)
                            (values user)))))))


(defun get-password-hash (password)
  (sha1-hex (string-to-octets password :external-format :utf-8)))
