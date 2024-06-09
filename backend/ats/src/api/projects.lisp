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
                #:require-role)
  (:import-from #:mito
                #:deftable)
  (:import-from #:serapeum
                #:soft-list-of))
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


(deftable project-with-jobs-count (project)
  ((jobs-count :initarg :jobs-count
               :type integer
               :col-type :integer
               :documentation "Количество открытых вакансий в проекте."
               :accessor jobs-count))
  (:table "ats.project"))


(define-rpc-method (ats-api get-projects) ()
  (:summary "Отдаёт все проекты")
  (:result (soft-list-of 'project-with-jobs-count))
  (with-connection ()
    (mito:select-by-sql 'project-with-jobs-count
                        "
with jobs_counters as (
select project_id as id, count(*) as jobs_count
from ats.job
where active = True
group by project_id
)
select p.*, coalesce(c.jobs_count, 0) as jobs_count
from ats.project as p
left outer join jobs_counters as c using (id)
")))
