(uiop:define-package #:passport/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:passport/core
                #:start-in-production))
(in-package #:passport/main)


(defmain (main) ()
  (start-in-production))
