(uiop:define-package #:common/rpc
  (:use #:cl)
  (:import-from #:openrpc-server
                #:slots-to-exclude
                #:type-to-schema)
  (:import-from #:local-time
                #:timestamp)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:mito.dao.table
                #:dao-table-class)
  (:import-from #:alexandria
                #:write-string-into-file)
  (:import-from #:closer-mop
                #:slot-definition-type
                #:slot-definition-name
                #:class-slots
                #:ensure-finalized)
  (:import-from #:alexandria
                #:write-string-into-file
                #:symbolicate)
  (:import-from #:dexador)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:mito
                #:save-dao)
  (:import-from #:common/permissions
                #:assert-can-delete
                #:assert-can-modify)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:export
   #:define-update-method
   #:define-delete-method))
(in-package #:common/rpc)


(defmethod type-to-schema ((obj (eql (find-class 'timestamp))))
  (dict "type" "string"))


(defmethod slots-to-exclude ((obj dao-table-class))
  (list* "synced"
         (call-next-method)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slots-with-types (class-name)
    (let* ((class (ensure-finalized (find-class class-name)))
           (slots (class-slots class)))
      (loop for slot in slots
            for name = (slot-definition-name slot)
            for type = (slot-definition-type slot)
            collect (list name type)))))


(defmacro define-update-method ((api method-name model) fields &body body)
  (let* ((types (slots-with-types model))
         (docstring (or (and body
                             (typep (first body)
                                    'string)
                             (first body))
                        ""))
         (positional-args (loop for name in fields
                                when (string-equal name "id")
                                collect name))
         (fields-kwargs (loop for name in fields
                              for given-name = (symbolicate name "-GIVEN-P")
                              unless (string-equal name "id")
                              collect (list name nil given-name)))
         (update-code (loop for (name default given-name) in fields-kwargs
                            ;; In the TYPES alist, first item is a symbol,
                            ;; corresponding to the slot name
                            for slot-name = (first (assoc name types :test #'string-equal))
                            ;; TODO: use accessors here instead of slot-value
                            collect `(when ,given-name
                                       (setf (slot-value object ',slot-name)
                                             ,name))))
         (param-definitions (loop for name in fields
                                  for type = (second (assoc name types :test #'string-equal))
                                  unless type
                                  do (error "Unable to find type for field ~S."
                                            name)
                                  collect (list :param name type))))
    `(define-rpc-method (,api ,method-name) (,@positional-args &key ,@fields-kwargs)
       ,@param-definitions
       (:result ,model)
       (:summary ,docstring)
       (with-session ((user-id scopes))
         (with-connection ()
           (let ((object (progn ,@body)))
             (assert-can-modify user-id object scopes)
             ,@update-code
             (save-dao object)
             (values object)))))))


(defmacro define-delete-method ((api method-name model) &body body)
  (let ((docstring (or (and body
                            (typep (first body)
                                   'string)
                            (first body))
                       "")))
    `(define-rpc-method (,api ,method-name) (id)
       (:summary ,docstring)
       (:param id integer "ID объекта.")
       (:result null)
       (with-session ((user-id scopes))
         (with-connection ()
           (let ((object (mito:find-dao ',model
                                        :id id)))
             (assert-can-delete user-id object scopes)
             (mito:delete-dao object)
             (values nil)))))))
