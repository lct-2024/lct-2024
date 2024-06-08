(uiop:define-package #:common/rpc
  (:use #:cl)
  (:import-from #:openrpc-server
                #:type-to-schema)
  (:import-from #:local-time
                #:timestamp)
  (:import-from #:serapeum
                #:dict))
(in-package #:common/rpc)


(defmethod type-to-schema (timestamp)
  (dict "type" "string"))
