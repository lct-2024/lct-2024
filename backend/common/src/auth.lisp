(uiop:define-package #:common/auth
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:export
   #:require-role))
(in-package #:common/auth)


(defun require-scope (user-id scopes required-scope action-name)
  (unless (member required-scope scopes :test #'string-equal)
    (with-fields (:user-id user-id
                  :scopes scopes
                  :required-scope required-scope
                  :action-name action-name)
      (log:error "AUTH: Unauthorized access")
      (return-error (fmt "Scope ~A is required to ~A."
                         required-scope
                         action-name)
                    :code 3))))
