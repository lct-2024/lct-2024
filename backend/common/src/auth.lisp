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


(defun require-role (user-id roles required-role action-name)
  (unless (member required-role roles)
    (with-fields (:user-id user-id
                  :roles roles
                  :required-role required-role
                  :action-name action-name)
      (log:error "AUTH: Unauthorized access")
      (return-error (fmt "Role ~A is required to ~A."
                         required-role
                         action-name)
                    :code 3))))
