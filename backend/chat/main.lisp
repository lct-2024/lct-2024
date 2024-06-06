(uiop:define-package #:chat/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:chat/server
                #:start-in-production))
(in-package #:chat/main)


(defmain (main) ()
  (start-in-production))
