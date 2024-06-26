(uiop:define-package #:ats/api/applicants
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:current-jwt-token
                #:with-session)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:ats/models/education
                #:get-education-simple
                #:update-education-simple
                #:education)
  (:import-from #:mito
                #:includes
                #:object-id
                #:select-by-sql
                #:deftable
                #:find-dao
                #:select-dao)
  (:import-from #:sxql
                #:where
                #:order-by)
  (:import-from #:ats/models/recommendation
                #:recommendation)
  (:import-from #:passport/client
                #:my-profile)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of)
  (:import-from #:common/dates
                #:parse-date)
  (:import-from #:ats/algorithms/resume-score
                #:update-user-scores-in-thread)
  (:import-from #:ats/models/job-applicant
                #:job-applicant-interview-date
                #:job-applicant-application-step
                #:job-applicant)
  (:import-from #:ats/models/application-step
                #:application-step-title
                #:application-step-num
                #:application-step)
  (:import-from #:common/auth
                #:require-scope)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:common/chat
                #:post-to-chat)
  (:import-from #:ats/models/applicant-skill
                #:get-skills-simple
                #:update-skills-simple)
  (:import-from #:local-time
                #:timestamp)
  (:export
   #:notify-applicant))
(in-package #:ats/api/applicants)


(deftable applicant-with-status ()
  ((fio :initarg :fio
        :col-type :text)
   (date :initarg :date
         :col-type :text)
   (step :initarg :step
         :col-type (or :null :text)
         :reader application-step)
   (score :initarg :score
          :col-type :integer)
   (application-type :initarg :application-type
                     :col-type :text
                     :reader application-type)))


(defclass applicant-group ()
  ((title :initarg :title
          :type string)
   (applicants :initarg :applicants
               :type (soft-list-of applicant-with-status))))


(defparameter *retrieve-applicants-query*
  "
select ja.id
     , a.name as fio
     , ja.updated_at::date::text as date
     , ast.title as step
     , (100 * random())::integer as score
     , ja.type as application_type
  from ats.applicant as a
join ats.job_applicant as ja on a.id = ja.applicant_id
left join ats.application_step as ast on ja.application_step_id = ast.id
where ja.job_id = ?
")

(defparameter *retrieve-single-applicant-query*
  "
select ja.id
     , a.name as fio
     , ja.updated_at::date::text as date
     , ast.title as step
     , (100 * random())::integer as score
     , ja.type as application_type
  from ats.applicant as a
join ats.job_applicant as ja on a.id = ja.applicant_id
left join ats.application_step as ast on ja.application_step_id = ast.id
where ja.id = ?
")

(define-rpc-method (ats-api get-applicants) (job-id)
  (:summary "Отдаёт кандидатов на вакансию.")
  (:param job-id integer)
  (:result (soft-list-of applicant-group))
  
  (with-connection ()
    (loop with groups = (serapeum:dict)
          for applicant in (select-by-sql 'applicant-with-status
                                          *retrieve-applicants-query*
                                          :binds (list job-id))
          for type = (application-type applicant)
          for group-id = (cond
                           ((application-step applicant)
                            "in-progress")
                           (t
                            (cond
                              ((or (string-equal type
                                                 "self-applied")
                                   (string-equal type
                                                 "recommended"))
                               type)
                              (t
                               (log:warn "Unknown application type " type " for job application " (object-id applicant))
                               "self-applied"))))
          do (push applicant
                   (gethash group-id
                            groups))
          finally (return (list (make-instance 'applicant-group
                                               :title "В работе"
                                               :applicants (gethash "in-progress" groups))
                                (make-instance 'applicant-group
                                               :title "Отклики"
                                               :applicants (gethash "self-applied" groups))
                                (make-instance 'applicant-group
                                               :title "Рекомендации"
                                               :applicants (gethash "recommended" groups)))))))


(define-rpc-method (ats-api get-applicant) (applicant-id)
  (:summary "Отдаёт основные данные о кандидате")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:result applicant)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (find-dao 'applicant
                 :id applicant-id)))))


(deftable extended-applicant (applicant)
  ((skills :initarg :skills
           :type (soft-list-of string)
           :col-type :array
           :documentation "Навыки кандидата.")
   (education :initarg :education
              :type (soft-list-of string)
              :col-type :array
              :documentation "Образование кандидата в упрощённом виде - только названия учебных заведений.")))


(defun extend-applicant (applicant)
  (change-class applicant 'extended-applicant
                :skills (get-skills-simple applicant)
                :education (get-education-simple applicant)))


(define-rpc-method (ats-api my-cv) ()
  (:summary "Отдаёт данные резюме текущего пользователя.")
  (:description "Этот метод вернёт none, если данные ещё не добавлены. Добавить их можно методом create-cv.

Метод требует аутентификации через заголовок Authorization.")
  (:result extended-applicant)
  (with-connection ()
    (with-session (user-id)
      (let* ((passport-client (passport/client:connect (current-jwt-token)))
             (profile (my-profile passport-client))
             (passport-fio (passport/client:user-fio profile))
             (passport-email (passport/client:user-email profile)))
        (extend-applicant
         (or (find-dao 'applicant
                       :user-id user-id)
             ;; Если нет CV то создадим
             (prog1 (mito:create-dao 'applicant
                                     :user-id user-id
                                     :email passport-email
                                     :name passport-fio
                                     :experience ""
                                     :about ""
                                     :contacts nil)
               (update-user-scores-in-thread user-id))))))))


(define-rpc-method (ats-api create-cv) (&key name email (experience "") (about "") contacts)
  (:summary "Добавляет данные резюме текущего пользователя.")
  (:description "Метод требует аутентификации через заголовок Authorization.

Если данные уже есть в базе, то будет возвращена ошибка - для их редактирования надо использовать метод edit-cv.")
  (:param name string "ФИО кандидата")
  (:param email string "Email для связи")
  (:param experience string "Описание опыта работы кандидата.")
  (:param about string "Общее описание кандидата, его увелечения, свойства характера и прочее.")
  (:param contacts (soft-list-of hash-table)
          "Список контактов в виде словарей с ключами \"type\" и \"value\", где значениями являются строки. Например: [{\"telegram\": \"telegram-nick\"}].")
  (:result applicant)
  
  (with-connection ()
    (with-session (user-id)
      (let* ((passport-client (passport/client:connect (current-jwt-token)))
             (profile (my-profile passport-client))
             (passport-fio (passport/client:user-fio profile))
             (passport-email (passport/client:user-email profile))
             (exists (mito:find-dao 'applicant
                                    :user-id user-id)))
        (when exists
          (openrpc-server:return-error "CV already exists."))
        
        (prog1 (mito:create-dao 'applicant
                                :user-id user-id
                                :email (or email passport-email)
                                :name (or name passport-fio)
                                :experience experience
                                :about about
                                :contacts contacts)
          (update-user-scores-in-thread user-id))))))


(define-rpc-method (ats-api update-cv) (&key name email experience about contacts education skills salary portfolio)
  (:summary "Обновляет данные резюме текущего пользователя.")
  (:description "Метод требует аутентификации через заголовок Authorization.

                 Если данных ещё нет в базе, то будет возвращена ошибка - для их добавления надо использовать метод create-cv.")
  (:param name string "ФИО кандидата")
  (:param email string "Email для связи")
  (:param experience string "Описание опыта работы кандидата.")
  (:param about string "Общее описание кандидата, его увелечения, свойства характера и прочее.")
  (:param portfolio string "Ссылка на portfolio")
  (:param salary string "Желаемая зарплата")
  (:param skills (soft-list-of string) "Список навыков")
  (:param education (soft-list-of string) "Список образовательных учреждений")
  (:param contacts (soft-list-of hash-table)
          "Список контактов в виде словарей с ключами \"type\" и \"value\", где значениями являются строки. Например: [{\"telegram\": \"telegram-nick\"}].")
  (:result extended-applicant)
  
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))


        (update-skills-simple applicant skills)
        (update-education-simple applicant education)
        
        (when salary
          (setf (ats/models/applicant::applicant-salary applicant)
                salary))
        (when portfolio
          (setf (ats/models/applicant::applicant-portfolio applicant)
                portfolio))
        (when name
          (setf (ats/models/applicant::applicant-name applicant)
                name))
        (when email
          (setf (ats/models/applicant::applicant-email applicant)
                email))
        (when experience
          (setf (ats/models/applicant::applicant-experience applicant)
                experience))
        (when about
          (setf (ats/models/applicant::applicant-about applicant)
                about))
        (when contacts
          (setf (ats/models/applicant::applicant-contacts applicant)
                contacts))
        (mito:save-dao applicant)
        (update-user-scores-in-thread user-id)
        (extend-applicant applicant)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define-rpc-method (ats-api get-applicant-education) (applicant-id)
  (:summary "Отдаёт записи об образовании кандидата")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:result (serapeum:soft-list-of education))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (select-dao 'education
         (where (:= :applicant-id applicant-id))
         (order-by :from))))))


(define-rpc-method (ats-api get-cv-education) ()
  (:summary "Отдаёт записи об образовании текущего пользователя")
  (:result (serapeum:soft-list-of education))
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
      
        (values
         (select-dao 'education
           (where (:= :applicant-id (mito:object-id applicant)))
           (order-by :from)))))))


(define-rpc-method (ats-api add-applicant-education) (applicant-id title from &key
                                                                   (type "")
                                                                   to
                                                                   speciality-id)
  (:summary "Добавляет новую запись об образовании кандидата")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:param title string "Название учебного заведения.")
  (:param from string "Дата начала обучения в формате YYYY-MM-DD.")
  (:param to string "Дата начала окончания обучения в формате YYYY-MM-DD. Если не окончено, то можно передать none.")
  (:param type string "Тип образования: высшее, среднее, незаконенное высше, курс, и тд.")
  (:param speciality-id integer "ID специальности по которой обучался кандидат. Или none.")
  (:result education)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (mito:create-dao 'education
                        :applicant-id applicant-id
                        :title title
                        :from (parse-date from)
                        :to (when to
                              (parse-date to))
                        :type type
                        :speciality-id speciality-id)))))


(define-rpc-method (ats-api add-cv-education) (title from &key
                                                     (type "")
                                                     to
                                                     speciality-id)
  (:summary "Добавляет новую запись об образовании текущего пользователя")
  (:param title string "Название учебного заведения.")
  (:param from string "Дата начала обучения в формате YYYY-MM-DD.")
  (:param to string "Дата начала окончания обучения в формате YYYY-MM-DD. Если не окончено, то можно передать none.")
  (:param type string "Тип образования: высшее, среднее, незаконенное высше, курс, и тд.")
  (:param speciality-id integer "ID специальности по которой обучался кандидат. Или none.")
  (:result education)
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
        
        (prog1
            (mito:create-dao 'education
                             :applicant applicant
                             :title title
                             :from (parse-date from)
                             :to (when to
                                   (parse-date to))
                             :type type
                             :speciality-id speciality-id)
          (update-user-scores-in-thread user-id))))))


(define-rpc-method (ats-api delete-applicant-education) (education-id)
  (:summary "Удаляет запись об образовании кандидата")
  (:param education-id integer "ID записи об образовании.")
  (:result null)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (mito:execute-sql "delete from ats.education where id = ?"
                        (list education-id))
      (values nil))))


(define-rpc-method (ats-api delete-cv-education) (education-id)
  (:summary "Удаляет запись об образовании кандидата")
  (:param education-id integer "ID записи об образовании.")
  (:result null)
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
      
        (mito:execute-sql "delete from ats.education where id = ? and applicant_id = ?"
                          (list education-id
                                (mito:object-id applicant)))
        (update-user-scores-in-thread user-id)
        (values nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recommendations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-rpc-method (ats-api get-applicant-recommendations) (applicant-id)
  (:summary "Отдаёт записи об рекоммендациях кандидата")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:result (serapeum:soft-list-of recommendation))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (select-dao 'recommendation
         (where (:= :applicant-id applicant-id))
         (order-by :fio))))))


(define-rpc-method (ats-api get-cv-recommendations) ()
  (:summary "Отдаёт записи об рекоммендациях текущего пользователя")
  (:result (serapeum:soft-list-of recommendation))
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
       
        (let ((results (select-dao 'recommendation
                         (where (:= :applicant-id (mito:object-id applicant)))
                         (order-by :fio))))
          (values results))))))


(define-rpc-method (ats-api add-applicant-recommendation) (applicant-id fio &key
                                                                        (position "")
                                                                        (company "")
                                                                        (email "")
                                                                        (phone ""))
  (:summary "Добавляет новую запись о рекомендации кандидата")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:param fio string "ФИО")
  (:param position string "Должность")
  (:param company string "Компания")
  (:param email string "Email")
  (:param phone string "Телефон")
  (:result recommendation)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (mito:create-dao 'recommendation
                        :applicant-id applicant-id
                        :fio fio
                        :position position
                        :company company
                        :email email
                        :phone phone)))))


(define-rpc-method (ats-api add-cv-recommendation) (fio &key
                                                        (position "")
                                                        (company "")
                                                        (email "")
                                                        (phone ""))
  (:summary "Добавляет новую запись о рекомендации кандидата")
  (:param fio string "ФИО")
  (:param position string "Должность")
  (:param company string "Компания")
  (:param email string "Email")
  (:param phone string "Телефон")
  (:result recommendation)
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
        (prog1
            (mito:create-dao 'recommendation
                             :applicant applicant
                             :fio fio
                             :position position
                             :company company
                             :email email
                             :phone phone)
          (update-user-scores-in-thread user-id))))))


(define-rpc-method (ats-api delete-applicant-recommendation) (recommendation-id)
  (:summary "Удаляет запись о рекомендации кандидата")
  (:param recommendation-id integer "ID рекомендации")
  (:result null)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (mito:execute-sql "delete from ats.recommendation where id = ?"
                        (list recommendation-id))
      (values nil))))


(define-rpc-method (ats-api delete-cv-recommendation) (recommendation-id)
  (:summary "Удаляет запись о рекомендации текущего пользователя")
  (:param recommendation-id integer "ID рекомендации")
  (:result null)
  (with-connection ()
    (with-session (user-id)
      (let* ((applicant (mito:find-dao 'applicant
                                       :user-id user-id)))
        (unless applicant
          (openrpc-server:return-error "CV does not exists yet."))
        (mito:execute-sql "delete from ats.recommendation where id = ? and applicant_id = ?"
                          (list recommendation-id
                                (mito:object-id applicant)))
        (update-user-scores-in-thread user-id)
        (values nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skills
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-rpc-method (ats-api get-cv-recommendations) ()
;;   (:summary "Отдаёт записи об рекоммендациях текущего пользователя")
;;   (:result (serapeum:soft-list-of recommendation))
;;   (with-connection ()
;;     (with-session (user-id)
;;       (let* ((applicant (mito:find-dao 'applicant
;;                                        :user-id user-id)))
;;         (unless applicant
;;           (openrpc-server:return-error "CV does not exists yet."))
       
;;         (let ((results (select-dao 'recommendation
;;                          (where (:= :applicant-id (mito:object-id applicant)))
;;                          (order-by :fio))))
;;           (values results))))))


;; (define-rpc-method (ats-api add-cv-recommendation) (fio &key
;;                                                         (position "")
;;                                                         (company "")
;;                                                         (email "")
;;                                                         (phone ""))
;;   (:summary "Добавляет новую запись о рекомендации кандидата")
;;   (:param fio string "ФИО")
;;   (:param position string "Должность")
;;   (:param company string "Компания")
;;   (:param email string "Email")
;;   (:param phone string "Телефон")
;;   (:result recommendation)
;;   (with-connection ()
;;     (with-session (user-id)
;;       (let* ((applicant (mito:find-dao 'applicant
;;                                        :user-id user-id)))
;;         (unless applicant
;;           (openrpc-server:return-error "CV does not exists yet."))
;;         (prog1
;;             (mito:create-dao 'recommendation
;;                              :applicant applicant
;;                              :fio fio
;;                              :position position
;;                              :company company
;;                              :email email
;;                              :phone phone)
;;           (update-user-scores-in-thread user-id))))))

;; (define-rpc-method (ats-api delete-cv-recommendation) (recommendation-id)
;;   (:summary "Удаляет запись о рекомендации текущего пользователя")
;;   (:param recommendation-id integer "ID рекомендации")
;;   (:result null)
;;   (with-connection ()
;;     (with-session (user-id)
;;       (let* ((applicant (mito:find-dao 'applicant
;;                                        :user-id user-id)))
;;         (unless applicant
;;           (openrpc-server:return-error "CV does not exists yet."))
;;         (mito:execute-sql "delete from ats.recommendation where id = ? and applicant_id = ?"
;;                           (list recommendation-id
;;                                 (mito:object-id applicant)))
;;         (update-user-scores-in-thread user-id)
;;         (values nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notify-applicant (applicant message)
  (ignore-errors
   (with-log-unhandled ()
     (let ((system-chat-id
             (getf (first
                    (mito:retrieve-by-sql "
select system_chat_id as id
from ats.applicant
where id = ?"
                                          :binds (list (etypecase applicant
                                                         (applicant (object-id applicant))
                                                         (integer applicant)))))
                   :id)))
       (when system-chat-id
         (post-to-chat system-chat-id
                       message))))))


(defun %move-to-the-next-step (job-id applicant-id)
  (let* ((application
           (first
            (mito:select-dao 'job-applicant
              (includes 'application-step)
              (where (:and
                      (:=
                       :job-id job-id)
                      (:=
                       :applicant-id applicant-id)))))))
    (unless application
      (openrpc-server:return-error "Person has no application to the job."))

    (cond
      ((job-applicant-application-step application)
       (let ((next-step
               (mito:find-dao 'ats/models/application-step::application-step
                              :num (1+
                                    (application-step-num
                                     (job-applicant-application-step application))))))
         ;; Продвигать можно лишь до последнего шага,
         ;; а там уже либо отказывать, либо делать оффер
         (when next-step
           (setf (job-applicant-application-step application)
                 next-step)
           (setf (job-applicant-interview-date application)
                 nil)

           (notify-applicant applicant-id
                             (fmt "Новый этап: ~A"
                                  (application-step-title next-step))))))
      (t
       (setf (job-applicant-application-step application)
             (mito:find-dao 'ats/models/application-step::application-step
                            :num 1))))
    (mito:save-dao application)

    (first
     (select-by-sql 'applicant-with-status
                    *retrieve-single-applicant-query*
                    :binds (list (mito:object-id application))))))


(define-rpc-method (ats-api move-to-the-next-step) (job-id applicant-id)
  (:summary "Переводит Соискателя на следующий этап в воронке собеседований для указанной вакансии.")
  (:param job-id integer "ID вакансии")
  (:param applicant-id integer "ID резюме соискателя")
  (:result applicant-with-status)
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.edit" "push applicant forward")
      
      (%move-to-the-next-step job-id applicant-id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Работа с назначением собеседований
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define-rpc-method (ats-api set-interview-date) (job-id applicant-id interview-date)
  (:summary "Сохраняет дату интервью")
  (:param job-id integer "ID вакансии")
  (:param applicant-id integer "ID резюме соискателя")
  (:param interview-date string "Время начала собеседования в формате ISO 8601")
  (:result t)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.edit" "set interview date")
      (mito:execute-sql "
update ats.job_applicant
   set interview_date = ?
 where job_id = ?
   and applicant_id = ?"
                        (list interview-date job-id applicant-id))
      (values t))))


(define-rpc-method (ats-api cancel-interview) (job-id applicant-id)
  (:summary "Отменяет интервью и сбрасывает дату интервью")
  (:param job-id integer "ID вакансии")
  (:param applicant-id integer "ID резюме соискателя")
  (:result t)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.job.edit" "set interview date")
      (mito:execute-sql "
update ats.job_applicant
   set interview_date = NULL
 where job_id = ?
   and applicant_id = ?"
                        (list job-id applicant-id))
      (values t))))


(deftable interview-details ()
  ((job-title :initarg :job-title
              :type string
              :col-type :text
              :documentation "Название вакансии")
   (date :initarg :date
         :type timestamp
         :col-type :timestamptz
         :documentation "Дата начала собеседования в формате ISO 8601")
   (name :initarg :name
         :type string
         :col-type :text
         :documentation "Имя кандидата")))


(defun user-interviews (user-id)
  (with-connection ()
    (mito:select-by-sql 'interview-details
                        "
select j.title
     , ja.interview_date as date
     , ap.name
  from ats.job as j
  join ats.job_applicant as ja on j.id = ja.job_id
  join ats.applicant as ap on ja.applicant_id = ap.id
 where ap.user_id = ?
   and ja.interview_date is not null
 order by ja.interview_date
"
                        :binds (list user-id))))


(define-rpc-method (ats-api my-interview) ()
  (:summary "Отдаёт Соискателю список дат на которые запланированы собеседования.")
  (:result (soft-list-of interview-details))
  
  (with-session ((user-id))
    (user-interviews user-id)))


(defun hr-interviews ()
  (with-connection ()
    (mito:select-by-sql 'interview-details
                        "
select j.title
     , ja.interview_date as date
  from ats.job as j
  join ats.job_applicant as ja on j.id = ja.job_id
  join ats.applicant as ap on ja.applicant_id = ap.id
 where ja.interview_date is not null
 order by ja.interview_date
")))


(define-rpc-method (ats-api all-interview) ()
  (:summary "Отдаёт HR список дат на которые запланированы собеседования.")
  (:description "Если метод запрашивает пользователь, не имеющий HR роли, то отдаёт ошибку.")
  (:result (soft-list-of interview-details))
  
  (with-session ((user-id scopes))
    (require-scope user-id scopes "ats.job.edit" "view interview list")
    (hr-interviews)))
