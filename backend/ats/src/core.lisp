(uiop:define-package #:ats
  (:use #:cl)
  (:nicknames #:ats/core)
  (:import-from #:40ants-openrpc)
  (:import-from #:40ants-openrpc/server)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:openrpc-server
                #:debug-on)
  (:export #:start
           #:stop
           #:start-in-production))
(in-package #:ats)


(defparameter *port* 10113)


(defun start ()
  (debug-on)
  (40ants-openrpc/server:start :api ats-api
                               :port *port*))

(defun stop ()
  (40ants-openrpc/server:stop :port *port*))


(defun start-in-production ()
  (40ants-openrpc/server:start-in-production :api ats-api))
