(uiop:define-package #:passport/api
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
  (:export #:passport-api))
(in-package #:passport/api)


(openrpc-server:define-api (passport-api :title "Passport API"))

