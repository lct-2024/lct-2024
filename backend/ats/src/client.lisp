(uiop:define-package #:ats/client
  (:use #:cl)
  (:import-from #:40ants-openrpc/client
                #:generate-client)
  (:shadow #:step))
(in-package #:ats/client)


(generate-client ats
                 "http://localhost:10113/")
