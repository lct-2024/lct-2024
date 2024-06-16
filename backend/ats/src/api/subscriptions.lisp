(uiop:define-package #:ats/api/subscriptions
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/city
                #:city)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:mito
                #:retrieve-dao
                #:find-dao
                #:select-dao)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/models/subscription
                #:subscription))
(in-package #:ats/api/subscriptions)


(define-rpc-method (ats-api get-subscriptions) ()
  (:summary "Отдаёт подписки текущего пользователя")
  (:result (serapeum:soft-list-of subscription))
  (with-connection ()
    (with-session (user-id)
      (let ((applicant (find-dao 'applicant
                                 :user-id user-id)))
        (when applicant
          (values (retrieve-dao 'subscription
                                :applicant applicant)))))))


(define-rpc-method (ats-api add-subscription) (&key speciality city project category)
  (:summary "Добавляет подписку на новые вакансии.")
  (:param city string "Город в котором идёт найм")
  (:param speciality string "Специальность")
  (:param project string "Проект")
  (:param category string "Категория")
  (:result (or null subscription))
  
  (with-connection ()
    (with-session (user-id)
      (let ((applicant (find-dao 'applicant
                                 :user-id user-id)))
        (when applicant
          (values
           (ats/models/subscription::add-subscription 
            applicant
            :speciality speciality
            :city city
            :project project
            :category category)))))))
