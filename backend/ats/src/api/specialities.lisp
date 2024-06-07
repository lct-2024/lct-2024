(uiop:define-package #:ats/api/specialities
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/speciality
                #:speciality)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:mito
                #:select-dao)
  (:import-from #:sxql
                #:order-by))
(in-package #:ats/api/specialities)


(define-rpc-method (ats-api get-specialities) ()
  (:summary "Отдаёт все специальности")
  (:result (serapeum:soft-list-of speciality))
  (with-connection ()
    (select-dao 'speciality
      (order-by :title))))


(define-rpc-method (ats-api suggest-specialities) (query)
  (:summary "Отдаёт специальности содержащие заданную подстроку.")
  (:param query string "Неполный ввод пользователя по которому надо сделать поиск.")
  (:result (serapeum:soft-list-of speciality))
  (with-connection ()
    (ats/models/speciality::suggest-specialities query)))
