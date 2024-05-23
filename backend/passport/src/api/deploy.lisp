(uiop:define-package #:passport/api/deploy
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
                #:get-next-user-id
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
                #:format-rfc3339-timestring)
  (:import-from #:bordeaux-threads-2
                #:make-thread))
(in-package #:passport/api/deploy)


(defvar *thread* nil)


(defun deploy-new-version ()
  (log:info "Deploying a new version of the frontend.")
  
  (unwind-protect
       (uiop:run-program "~/projects/lct-2024/deploy/update-frontend.sh")
    (setf *thread* nil)))


(defun deploy-in-thread ()
  (cond
    (*thread*
     (log:warn "Deployment is already in progress.")
     (values "Deployment is already in progress."))
    (t
     (setf *thread*
           (make-thread #'deploy-new-version))
     (values "Deploy was started"))))


(define-rpc-method (passport-api deploy) (token)
  (:summary "Запускает деплой фронтенда.")
  (:param token string)
  (:result string)

  (cond
    ((string-equal (uiop:getenv "DEPLOY_SECRET")
                   token)
  
     (deploy-in-thread))
    (t
     (log:error "Provided deploy token \"~A\" expected token from env variable." token)
     (fmt "Provided deploy token \"~A\" expected token is wrong." token))))

