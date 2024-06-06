(uiop:define-package #:ats-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:ats-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
