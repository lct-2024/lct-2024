(uiop:define-package #:ats/api/cities
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
                #:ats-api))
(in-package #:ats/api/cities)


(define-rpc-method (ats-api get-cities) ()
  (:summary "Отдаёт все города РФ.")
  (:result (serapeum:soft-list-of city))
  (with-connection ()
    (mito:select-dao 'city)))


(define-rpc-method (ats-api suggest-cities) (query)
  (:summary "Отдаёт города РФ содержащие заданную подстроку.")
  (:param query string "Неполный ввод пользователя по которому надо сделать поиск.")
  (:result (serapeum:soft-list-of city))
  (with-connection ()
    (ats/models/city::suggest-cities query)))
