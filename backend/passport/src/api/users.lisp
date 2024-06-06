(uiop:define-package #:passport/api/users
  (:use #:cl)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:mito
                #:save-dao
                #:find-dao)
  (:import-from #:passport/models/user
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
                #:fmt)
  (:import-from #:local-time
                #:now
                #:format-rfc3339-timestring))
(in-package #:passport/api/users)


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
        ((equal user-hash hash)
         (auth-log "User logged in: ~A" email)
         (issue-token-for user))
        (t
         (auth-log "User entered wrong pass or missing in db: ~A" email)
         (openrpc-server:return-error "Неправильный email или пароль." :code 1))))))


(define-rpc-method (passport-api signup) (email password fio
                                                &key
                                                my-role)
  (:summary "Регистрирует новую учётку с указанным email и паролем.")
  (:param email string)
  (:param password string)
  (:param fio string)
  (:param my-role string "Должность текущего пользователя в компании.")
  
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
                                     :position my-role
                                     :password-hash (get-password-hash password))))
         (auth-log "User signed up: ~A" email)
         (issue-token-for user)))
      (t
       (auth-log "User used email which already taken: ~A" email)
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(define-rpc-method (passport-api my-profile) ()
  (:summary "Отдаёт профиль текущего залогиненого пользователя.")
  (:result user)
  (with-connection ()
    (with-session (user-id)
      (first
       (mito:select-dao 'user
                        (sxql:where (:= :id user-id)))))))


(defun get-password-hash (password)
  (sha1-hex (string-to-octets password :external-format :utf-8)))
