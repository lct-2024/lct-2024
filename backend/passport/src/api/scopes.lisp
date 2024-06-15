(uiop:define-package #:passport/api/scopes
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
                #:select-dao
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
                #:format-rfc3339-timestring)
  (:import-from #:passport/models/scope
                #:scope)
  (:import-from #:sxql
                #:order-by))
(in-package #:passport/api/scopes)


(define-rpc-method (passport-api get-scopes) ()
  (:summary "Выдаёт известные в системе скоупы и их описания.")
  (:result (soft-list-of scope))
  (with-connection ()
    (select-dao 'scope
      (order-by :name))))
