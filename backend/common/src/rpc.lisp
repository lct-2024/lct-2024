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
                #:dao-table-class))
(in-package #:common/rpc)


(defmethod type-to-schema (timestamp)
  (dict "type" "string"))


(defmethod slots-to-exclude ((obj dao-table-class))
  (list* "synced"
         (call-next-method)))
