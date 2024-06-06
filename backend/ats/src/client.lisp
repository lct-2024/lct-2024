(uiop:define-package #:ats/client
  (:use #:cl)
  (:import-from #:40ants-openrpc/client
                #:generate-client))
(in-package #:ats/client)


(generate-client ats
                 "http://localhost:10113/")
