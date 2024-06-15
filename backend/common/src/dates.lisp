(uiop:define-package #:common/dates
  (:use #:cl)
  (:import-from #:local-time
                #:parse-timestring)
  (:import-from #:str
                #:containsp)
  (:import-from #:serapeum
                #:fmt)
  (:export
   #:parse-date))
(in-package #:common/dates)


(defun parse-date (date)
  (parse-timestring
   (cond
     ((and (typep date 'string)
           (containsp "-" date))
      date)
     (t
      ;; Когда указан только год
      (fmt "~A-01-01" date)))))
