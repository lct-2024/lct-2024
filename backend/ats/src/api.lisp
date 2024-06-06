(uiop:define-package #:ats/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-api)
  (:export #:ats-api))
(in-package #:ats/api)


(define-api (ats-api :title "Applicant Tracking System API"))

