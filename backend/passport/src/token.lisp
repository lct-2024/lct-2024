(uiop:define-package #:passport/token
  (:use #:cl)
  (:import-from #:cl-json-web-tokens)
  (:export #:issue-token))
(in-package #:passport/token)


(defun decode (token)
  (cl-json-web-tokens:decode token :secret (get-jwt-secret)))


(defun get-jwt-secret ()
  (or (uiop:getenv "JWT_SECRET")
      "test-secret"))


(defun issue-token (payload &key ttl)
  (check-type ttl (or null
                      integer))
  (cl-json-web-tokens:issue payload
                            :algorithm :hs256
                            :secret (get-jwt-secret)
                            :issued-at (get-universal-time)
                            ;; Если захотим, чтобы токены протухали через N минут
                            ;; :expiration (+ (get-universal-time)
                            ;;                (* 15 60))
                            ))
