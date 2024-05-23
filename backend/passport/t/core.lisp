(uiop:define-package #:passport-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:passport-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
