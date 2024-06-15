(uiop:define-package #:chat/client
  (:use #:cl)
  (:import-from #:40ants-openrpc/client
                #:generate-client))
(in-package #:chat/client)


(generate-client chat-api
                 "http://localhost:10112/")

