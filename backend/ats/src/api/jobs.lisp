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
                #:define-update-method))
(in-package #:ats/api/jobs)


(defmethod scope-for-editing ((obj job))
  "ats.job.edit")

(defmethod scope-for-deleting ((obj job))
  "ats.job.delete")


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


(define-rpc-method (ats-api get-jobs) (&key show-all)
  (:summary "Отдаёт все вакансии")
  (:param show-all boolean "Показать все вакансии, даже те что уже закрыты.")
  (:result (serapeum:soft-list-of job))
  (with-connection ()
    (with-session ((user-id) :require nil)
      (values
       (loop for job in (select-dao 'ats/models/job::job
                          (includes 'project)
                          (includes 'speciality)
                          (unless show-all
                            (where (:= :active 1))))
             collect
                (change-class job 'job
                              :skills (get-job-skills job)
                              :programming-languages (get-job-programming-languages job)
                              :resume-matching-score (if user-id
                                                         (calculate-resume-score job user-id)
                                                         0)))))))


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


(define-update-method (ats-api update-job job)
                      (id title category city description project-id speciality-id
                          ;; TODO: Поменять skills или programming-languages так пока нельзя
                          ;; потому что это many-to-many связь,
                          ;; надо будет с этим что-то придумать.
                          active active-to type-of-employment salary required-experience)
  "Обновить вакансию."
  (find-dao 'job
            :id id))


(define-delete-method (ats-api delete-job job)
  ;; TODO: по идее, как и при закрытии позиции, тут надо уметь уведомлять кандидатов
  ;; о том, что позиция закрылась.
  "Удалить вакансию.")
