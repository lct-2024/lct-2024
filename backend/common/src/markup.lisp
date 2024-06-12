(uiop:define-package #:common/markup
  (:use #:cl)
  (:import-from #:3bmd
                #:parse-string-and-print-to-stream)
  (:export #:markdown->html))
(in-package #:common/markup)


(defun markdown->html (text)
  (with-output-to-string (s)
    (parse-string-and-print-to-stream text s)))
