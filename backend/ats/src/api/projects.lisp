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
                #:includes
                #:deftable)
  (:import-from #:serapeum
                #:fmt
                #:dict
                #:soft-list-of)
  (:import-from #:ats/models/theme
                #:theme)
  (:import-from #:40ants-pg/utils
                #:select-dao-by-ids)
  (:import-from #:ats/models/project-theme
                #:project-theme)
  (:import-from #:sxql
                #:where))
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


(deftable project-with-themes (project-with-jobs-count)
  ((themes :initarg :themes
           :type (soft-list-of theme)
           :col-type :array
           :documentation "Темы к которым привязан проект."
           :accessor themes))
  (:table "ats.project"))


(define-rpc-method (ats-api get-projects) (&key theme-id)
  (:summary "Отдаёт все проекты")
  (:param theme-id (or null integer) "ID темы по которой нужно выбрать проекты. Если не указано, то будут отданы все.")
  (:result (soft-list-of 'project-with-themes))
  (with-connection ()
    (let* ((query                                          "
with jobs_counters as (
select project_id as id, count(*) as jobs_count
from ats.job
where active = True
group by project_id
)
select p.*, coalesce(c.jobs_count, 0) as jobs_count
from ats.project as p
left outer join jobs_counters as c using (id)
")
           (projects (mito:select-by-sql 'project-with-jobs-count
                                         (if theme-id
                                             (fmt "~A~%join ats.project_theme as pt on p.id = pt.project_id where pt.theme_id = ?"
                                                  query)
                                             query)
                                         :binds (when theme-id
                                                  (list theme-id))))
           (ids (mapcar #'mito:object-id projects))
           ;; TODO: выделить в отдельную функцию для заполнения many-to-many полей
           ;; может быть даже вытащить в Mito?
           (res (mito:select-dao 'project-theme
                  (includes 'theme)
                  (where (:in :project_id ids))))
           (themes-by-project-id
             (loop with result = (dict)
                   for project-theme in res
                   for theme = (ats/models/project-theme::theme project-theme)
                   for project-id = (slot-value project-theme 'ats/models/project-theme::project-id)
                   do (push theme (gethash project-id result))
                   finally (return result))))
      (loop for project in projects
            for themes = (gethash (mito:object-id project) themes-by-project-id)
            do (change-class project 'project-with-themes
                             :themes themes)
            collect project))))
