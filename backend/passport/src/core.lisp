(uiop:define-package #:passport
  (:use #:cl)
  (:nicknames #:passport/core)
  (:import-from #:40ants-openrpc)
  (:import-from #:40ants-openrpc/server)
  (:import-from #:passport/api
                #:passport-api)
  (:export #:start
           #:stop
           #:start-in-production))
(in-package #:passport)


(defparameter *port* 10111)


(defun start ()
  (openrpc-server:debug-on)
  (40ants-openrpc/server:start :api passport-api
                               :port *port*))

(defun stop ()
  (40ants-openrpc/server:stop :port *port*))


(defun start-in-production ()
  (40ants-openrpc/server:start-in-production :api passport-api))
