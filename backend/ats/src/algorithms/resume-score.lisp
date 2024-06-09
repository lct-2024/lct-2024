(uiop:define-package #:ats/algorithms/resume-score
  (:use #:cl)
  (:import-from #:serapeum
                #:href
                #:dict)
  (:import-from #:alexandria
                #:ensure-gethash))
(in-package #:ats/algorithms/resume-score)


(defvar *scores* (dict))

(defun calculate-resume-score (job user-id)
  "TODO: тут надо сделать реальный алгоритм, а пока пусть так."
  (let ((job-scores (ensure-gethash (mito:object-id job)
                                   *scores*
                                   (dict))))
    (ensure-gethash user-id
                    job-scores
                    (random 101))))
