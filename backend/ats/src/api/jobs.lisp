(uiop:define-package #:ats/api/jobs
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/job
                #:job)
  (:import-from #:ats/api
                #:ats-api))
(in-package #:ats/api/jobs)


(define-rpc-method (ats-api create-job) (title description)
  (:summary "Добавляет в базу новую вакансию")
  (:param title string "Название вакансии")
  (:param description string "Описание вакансии")
  (:result job)
  (break)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (break)
      (mito:create-dao 'job
                       :title title
                       :description description))))


(define-rpc-method (ats-api get-jobs) ()
  (:summary "Отдаёт все вакансии")
  (:result (serapeum:soft-list-of job))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (mito:select-dao 'job))))
