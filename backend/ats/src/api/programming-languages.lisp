(uiop:define-package #:ats/api/programming-languages
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/programming-language
                #:programming-language)
  (:import-from #:ats/api
                #:ats-api))
(in-package #:ats/api/programming-languages)


(define-rpc-method (ats-api get-programming-languages) ()
  (:summary "Отдаёт список языков программирования.")
  (:result (serapeum:soft-list-of programming-language))
  (with-connection ()
    (mito:select-dao 'programming-language)))


(define-rpc-method (ats-api suggest-programming-languages) (query)
  (:summary "Отдаёт языки программирования, содержащие заданную подстроку.")
  (:param query string "Неполный ввод пользователя по которому надо сделать поиск.")
  (:result (serapeum:soft-list-of programming-language))
  (with-connection ()
    (ats/models/programming-language::suggest-programming-languages query)))
