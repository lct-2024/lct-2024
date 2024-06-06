(uiop:define-package #:ats/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:ats/core
                #:start-in-production))
(in-package #:ats/main)


(defmain (main) ()
  (start-in-production))
