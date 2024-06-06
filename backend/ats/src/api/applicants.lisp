(uiop:define-package #:ats/api/applicants
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/api
                #:ats-api))
(in-package #:ats/api/applicants)


(define-rpc-method (ats-api get-applicants) ()
  (:summary "Отдаёт всех кандидатов")
  (:result (serapeum:soft-list-of applicant))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (mito:select-dao 'applicant))))
