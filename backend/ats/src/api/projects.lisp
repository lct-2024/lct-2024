(uiop:define-package #:ats/api/projects
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/project
                #:project)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:common/auth
                #:require-role))
(in-package #:ats/api/projects)


(define-rpc-method (ats-api create-project) (title description)
  (:summary "Добавляет в базу новый проект")
  (:param title string "Название проекта")
  (:param description string "Описание проекта")
  (:result project)
  (with-connection ()
    (with-session ((user-id roles))
      (require-role user-id roles :admin "create a project")
      
      (log:info "User" user-id "with roles" roles "creates a project" title)
      (mito:create-dao 'project
                       :title title
                       :description description))))


(define-rpc-method (ats-api get-projects) ()
  (:summary "Отдаёт все проекты")
  (:result (serapeum:soft-list-of project))
  (with-connection ()
    (mito:select-dao 'project)))
