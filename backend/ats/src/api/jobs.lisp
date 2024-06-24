(uiop:define-package #:ats/api/jobs
  (:use #:cl)
  (:import-from #:ats/models/job-applicant)
  (:import-from #:ats/models/job
                #:job-open)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:ats/models/project
                #:project
                #:get-project-by-id)
  (:import-from #:ats/models/speciality
                #:speciality
                #:get-speciality-by-id)
  (:import-from #:ats/models/job-skill
                #:get-job-skills
                #:bind-job-to-skills)
  (:import-from #:ats/models/skill
                #:skill)
  (:import-from #:ats/models/job-programming-language
                #:get-job-programming-languages
                #:bind-job-to-programming-languages)
  (:import-from #:mito
                #:object-id
                #:find-dao
                #:select-dao
                #:includes
                #:deftable)
  (:import-from #:ats/algorithms/resume-score
                #:calculate-resume-score)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration+)
  (:import-from #:local-time-duration
                #:duration)
  (:import-from #:sxql
                #:order-by
                #:left-join
                #:join
                #:where)
  (:import-from #:common/auth
                #:require-scope)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:common/permissions
                #:scope-for-editing
                #:scope-for-deleting)
  (:import-from #:common/rpc
                #:define-delete-method
                #:define-update-method)
  (:import-from #:40ants-pg/query
                #:select-one-column))
(in-package #:ats/api/jobs)


(deftable job (ats/models/job::job)
  ((skills :initarg :skills
           :reader job-skills
           :type (soft-list-of skill)
           :documentation "Список объектов типа Skill."
           :col-type :array)
   (programming-languages :initarg :programming-languages
                          :reader job-programming-languages
                          :col-type :array)
   (resume-matching-score :initarg :resume-matching-score
                          :type integer
                          :col-type :integer
                          :default 0
                          :documentation "На сколько процентов, резюме текущего пользователя соответствует этой вакансии (от 0 до 100)"
                          :reader resume-matching-score))
  (:table-name "ats.job"))


(defmethod scope-for-editing ((obj job))
  "ats.job.edit")

(defmethod scope-for-deleting ((obj job))
  "ats.job.delete")


(define-rpc-method (ats-api create-job) (title description project-id category
                                               speciality-id
                                               &key
                                               (type-of-employment "Полная")
                                               programming-language-ids
                                               (active t)
                                               active-to 
                                               skill-ids
                                               salary
                                               required-experience)
  (:summary "Добавляет в базу новую вакансию")
  (:param title string "Название вакансии")
  (:param category string "Категория вакансии: Разработка, Аналитика, Тестирование, Другое")
  (:param description string "Описание вакансии")
  (:param type-of-employment string "Вид занятости, например: \"Полная\", \"Частичная\", \"Стажировка\".")
  (:param project-id integer "ID проекта за которым закрепляется вакансия. Можно получить методом get_projects.")
  (:param speciality-id integer "ID специальности нужной для вакансии. Можно получить методом get_specialities.")
  (:param skill-ids (soft-list-of integer) "Список ID навыков.")
  (:param active boolean "Признак того, ведётся ли набор на вакансию.")
  (:param active-to local-time:timestamp "Время, до которого будет проводиться набор на вакансию.")
  (:param programming-language-ids (soft-list-of integer) "Список ID языков программирования.")
  (:param salary (or null string) "Примерный оклад, как цифра или диапазон.")
  (:param required-experience (or null string) "ID чата, привязанного к объекту.")
  (:result job)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.create" "create a job")
      
      (let* ((job (mito:create-dao 'ats/models/job::job
                                   :title title
                                   :description description
                                   :project (get-project-by-id project-id)
                                   :category category
                                   :speciality (when speciality-id
                                                 (get-speciality-by-id speciality-id))
                                   :active active
                                   :active-to (or active-to
                                                  (when active
                                                    (timestamp-duration+ (now)
                                                                         (duration :day 30))))
                                   :type-of-employment type-of-employment
                                   :required-experience required-experience
                                   :salary salary))
             (skills (bind-job-to-skills job skill-ids))
             (programming-languages (bind-job-to-programming-languages job programming-language-ids)))

        (change-class job 'job
                      :skills skills
                      :programming-languages programming-languages)
        (values job)))))

(defvar *applicant* nil)

(defun get-jobs-with-filter (&key city category applicant project speciality show-all)
  (let* ((query "
with scores as (
  select *
   from ats.score
  where applicant_id = ?
)
select j.*
  from ats.job as j
  join ats.speciality as s on j.speciality_id = s.id
  join ats.project as p on j.project_id = p.id
  left join scores on j.id = scores.job_id
where (? = 0 or active)
  and (? = 0 or city = ?)
  and (? = 0 or category = ?)
  and (? = 0 or s.title = ?)
  and (? = 0 or p.title = ?)
order by scores.total desc
")

         (binds (list
                 ;; тут нельзя поступить как с другими фильтрами, потому что если
                 ;; applicant нет, нам надо чтобы в scores не было строк вообще
                 (if applicant
                     (object-id applicant)
                     -1)
                 (if (not show-all) 1 0)
                 (if city 1 0) city
                 (if category 1 0) category
                 (if speciality 1 0) speciality
                 (if project 1 0) project)))
    (let ((results (mito:select-by-sql 'ats/models/job::job
                                       query
                                       :binds binds)))
      (mito:include-foreign-objects 'project results)
      (mito:include-foreign-objects 'speciality results)
      (values results))))



(defun get-jobs-with-filter-old (&key city category applicant project speciality show-all)
  (let* ((filters (remove-if #'null
                             (list (unless show-all
                                     '(:= :active 1))
                                   (when city
                                     `(:= :city ,city))
                                   (when category
                                     `(:= :category ,category))
                                   ;; (when applicant
                                   ;;   `(:or (:is-null :score.applicant_id)
                                   ;;         (:= :score.applicant_id
                                   ;;             ,(object-id applicant))))
                                   (when speciality
                                     `(:= :speciality.title
                                          ,speciality))
                                   (when project
                                     `(:= :project.title
                                          ,project)))))
         (filter-expression
           `(:and (:= 1 1)
                  ,@filters))
         (join-score-if-needed
           nil
           ;; (when applicant
           ;;   (left-join :ats.score
           ;;              :on (:= :job.id
           ;;                   :score.job_id)))
           )
         (join-speciality-if-needed
           (when speciality
             (left-join :ats.speciality
                        :on (:= :job.speciality_id
                             :speciality.id))))
         (join-project-if-needed
           (when project
             (left-join :ats.project
                        :on (:= :job.project_id
                             :project.id)))))
    (mito:select-dao 'ats/models/job::job
      (includes 'project)
      (includes 'speciality)
      join-score-if-needed
      join-speciality-if-needed
      join-project-if-needed
      (where filter-expression)
      ;; (when applicant
      ;;   (order-by (:desc :score.total)))
      )))


(define-rpc-method (ats-api get-jobs) (&key category city project speciality show-all)
  (:summary "Отдаёт все вакансии")
  (:description "Если вакансии смотрит соискатель, то они сортируются начиная от наиболее подходищих под его резюме.")
  (:param city string "Город. Список городов в которых есть вакансии можно получить из ats.get_job_cities.")
  (:param category string "Категория к которой отностится вакансия. Получить список категорий можно через метод ats.get_job_categories.")
  (:param speciality string "Специальность. Список получить из ats.get_job_specialities.")
  (:param project string "Специальность. Список получить из ats.get_job_projects.")
  (:param show-all boolean "Показать все вакансии, даже те что уже закрыты.")
  (:result (soft-list-of job))
  
  (with-connection ()
    (with-session ((user-id) :require nil)
      (let ((applicant (when user-id
                         (mito:find-dao 'applicant
                                        :user-id user-id))))
        (values
         (loop for job in (get-jobs-with-filter :city city
                                                :category category
                                                :show-all show-all
                                                :project project
                                                :speciality speciality
                                                :applicant applicant)
               collect
                  (change-class job 'job
                                :skills (get-job-skills job)
                                :programming-languages (get-job-programming-languages job)
                                :resume-matching-score (if applicant
                                                           (calculate-resume-score job
                                                                                   applicant)
                                                           0))))))))


(define-rpc-method (ats-api get-job-categories) ()
  (:summary "Отдаёт список категорий к которым может относиться вакансия")
  (:result (soft-list-of string))
  (with-connection ()
    (select-one-column "
SELECT distinct(category) as name
  from ats.job
 where job.active
order by 1"
                       :column :name)))


(define-rpc-method (ats-api get-job-cities) ()
  (:summary "Отдаёт все города для которых открыты вакансии.")
  (:result (soft-list-of string))
  (with-connection ()
    (select-one-column "
SELECT distinct(city) as name
  from ats.job
 where job.active
order by 1"
                       :column :name)))


(define-rpc-method (ats-api get-job-specialities) ()
  (:summary "Отдаёт все специальности для которых открыты вакансии.")
  (:result (soft-list-of string))
  (with-connection ()
    (select-one-column "
SELECT distinct(s.title) as name
  from ats.job as j
join ats.speciality as s on j.speciality_id = s.id
 where j.active
order by 1"
                       :column :name)))


(define-rpc-method (ats-api get-job-projects) ()
  (:summary "Отдаёт все проекты для которых открыты вакансии.")
  (:result (soft-list-of string))
  (with-connection ()
    (select-one-column "
SELECT distinct(s.title) as name
  from ats.job as j
join ats.project as s on j.project_id = s.id
 where j.active
order by 1"
                       :column :name)))


(define-rpc-method (ats-api apply-to-the-job) (job-id)
  (:summary "Откликнуться на вакансию")
  (:description "Откликнуться может только залогиненый пользователь. Так что для этого метода заголовок Authorization с токеном обязателен.

Кроме того, у текущего пользователя должна быть анкета с резюме и заполенной контактной информацией. Эту информацию можно добавить методом `create-cv` или получить методом `my-cv`.")
  (:param job-id integer "ID вакансии")
  (:result boolean)
  
  (with-connection ()
    (with-session ((user-id))
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id))
             (job (mito:find-dao 'job
                                 :id job-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
        
        (unless job
          (openrpc-server:return-error "Job was not found."))
        
        (ats/models/job-applicant::apply-to-the-job
         job
         (mito:object-id applicant)
         :type "self-applied")
        (values t)))))


(define-rpc-method (ats-api get-my-jobs) ()
  (:summary "Отдаёт вакансии на которые откликнулся текущий кандидат")
  (:result (serapeum:soft-list-of job))
  
  (with-connection ()
    (with-session ((user-id))
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (when applicant
          (loop for job in (select-dao 'ats/models/job::job
                             (includes 'project)
                             (includes 'speciality)
                             (join :ats.job_applicant
                                 :on (:=
                                      :job.id
                                      :job_applicant.job_id))
                             (where (:= :job_applicant.applicant_id
                                        (mito:object-id applicant))))
                collect
                   (change-class job 'job
                                 :skills (get-job-skills job)
                                 :programming-languages (get-job-programming-languages job)
                                 :resume-matching-score (if user-id
                                                            (calculate-resume-score job applicant)
                                                            0))))))))


(define-rpc-method (ats-api open-position) (job-id)
  (:summary "Открыть вакансию")
  (:description "Сделать это может только залогиненый пользователь со scope ats.job.edit")
  (:param job-id integer "ID вакансии")
  (:result boolean)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.edit" "open the position")
      
      (let* ((job (mito:find-dao 'job
                                 :id job-id)))
        (unless job
          (openrpc-server:return-error "Job was not found."))

        (setf (job-open job)
              t)
        (mito:save-dao job)
        (values t)))))


(define-rpc-method (ats-api close-position) (job-id)
  (:summary "Закрыть вакансию")
  (:description "Сделать это может только залогиненый пользователь со scope ats.job.edit")
  (:param job-id integer "ID вакансии")
  (:result boolean)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.edit" "close the position")
      
      (let* ((job (mito:find-dao 'job
                                 :id job-id)))
        (unless job
          (openrpc-server:return-error "Job was not found."))

        ;; TODO: надо ещё и уведомления всем текущим кандидатам рассылать
        (setf (job-open job)
              nil)
        (mito:save-dao job)
        (values t)))))


;; (define-update-method (ats-api update-job job)
;;                       (id title category city description project-id speciality-id
;;                           ;; TODO: Поменять skills или programming-languages так пока нельзя
;;                           ;; потому что это many-to-many связь,
;;                           ;; надо будет с этим что-то придумать.
;;                           active active-to type-of-employment salary required-experience)
;;   "Обновить вакансию."
;;   (find-dao 'job
;;             :id id))


;; (define-delete-method (ats-api delete-job job)
;;   ;; TODO: по идее, как и при закрытии позиции, тут надо уметь уведомлять кандидатов
;;   ;; о том, что позиция закрылась.
;;   "Удалить вакансию.")
