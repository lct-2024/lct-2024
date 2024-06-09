(uiop:define-package #:ats/api/jobs
  (:use #:cl)
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
                #:select-dao
                #:includes
                #:deftable)
  (:import-from #:ats/algorithms/resume-score
                #:calculate-resume-score))
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


(define-rpc-method (ats-api create-job) (title description project-id category
                                               &key
                                               speciality-id
                                               (type-of-employment "Полная")
                                               programming-language-ids
                                               skill-ids)
  (:summary "Добавляет в базу новую вакансию")
  (:param title string "Название вакансии")
  (:param category string "Категория вакансии: Разработка, Аналитика, Тестирование, Другое")
  (:param description string "Описание вакансии")
  (:param type-of-employment string "Вид занятости, например: \"Полная\", \"Частичная\", \"Стажировка\".")
  (:param project-id integer "ID проекта за которым закрепляется вакансия. Можно получить методом get_projects.")
  (:param speciality-id integer "ID специальности нужной для вакансии. Можно получить методом get_specialities.")
  (:param skill-ids (soft-list-of integer) "Список ID навыков.")
  (:param programming-language-ids (soft-list-of integer) "Список ID языков программирования.")
  (:result job)
  
  (with-connection ()
    (with-session ((user-id roles))
      (common/auth:require-role user-id roles :admin "create a job")
      
      (let* ((job (mito:create-dao 'ats/models/job::job
                                   :title title
                                   :description description
                                   :project (get-project-by-id project-id)
                                   :category category
                                   :speciality (get-speciality-by-id speciality-id)
                                   :type-of-employment type-of-employment))
             (skills (bind-job-to-skills job skill-ids))
             (programming-languages (bind-job-to-programming-languages job programming-language-ids)))

        (change-class job 'job
                      :skills skills
                      :programming-languages programming-languages)
        (values job)))))


(define-rpc-method (ats-api get-jobs) ()
  (:summary "Отдаёт все вакансии")
  (:result (serapeum:soft-list-of job))
  (with-connection ()
    (with-session ((user-id) :require nil)
      (values
       (loop for job in (select-dao 'ats/models/job::job
                          (includes 'project)
                          (includes 'speciality))
             collect
                (change-class job 'job
                              :skills (get-job-skills job)
                              :programming-languages (get-job-programming-languages job)
                              :resume-matching-score (when user-id
                                                       (calculate-resume-score job user-id))))))))
