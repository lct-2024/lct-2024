(uiop:define-package #:chat/server
  (:use #:cl)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/api)
  (:import-from #:chat/chat-member/api)
  (:import-from #:chat/message/api)
  (:import-from #:40ants-openrpc/server)
  (:import-from #:openrpc-server
                #:debug-on)
  (:export
   #:start-me
   #:stop-me))
(in-package #:chat/server)


(defparameter *port* 10112)


(defun start-me ()
  (debug-on)
  (40ants-openrpc/server:start :api chat-api
                               :port *port*))

(defun stop-me ()
  (40ants-openrpc/server:stop :port *port*))


(defun start-in-production ()
  (40ants-openrpc/server:start-in-production :api chat-api))
