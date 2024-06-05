(uiop:define-package #:chat/server
  (:use #:cl)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/api)
  (:import-from #:chat/chat-member/api)
  (:import-from #:chat/message/api)
  (:import-from #:40ants-openrpc/server)
  (:export
   #:start-me
   #:stop-me))
(in-package #:chat/server)


(defparameter *port* 10112)


(defun start-me ()
  (40ants-openrpc/server:start :api chat-api
                               :port *port*))

(defun stop-me ()
  (40ants-openrpc/server:stop :port *port*))
