(uiop:define-package #:ats/api/skills
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/skill
                #:skill)
  (:import-from #:ats/api
                #:ats-api))
(in-package #:ats/api/skills)


(define-rpc-method (ats-api get-skills) ()
  (:summary "Отдаёт все навыки в алфавитном порядке.")
  (:result (serapeum:soft-list-of skill))
  (with-connection ()
    (mito:select-dao 'skill)))


(define-rpc-method (ats-api suggest-skills) (query)
  (:summary "Отдаёт навыки, содержащие заданную подстроку.")
  (:param query string "Неполный ввод пользователя по которому надо сделать поиск.")
  (:result (serapeum:soft-list-of skill))
  (with-connection ()
    (ats/models/skill::suggest-skills query)))
