(uiop:define-package #:ats/api/score
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
  (:import-from #:mito
                #:find-dao
                #:select-dao
                #:includes
                #:deftable)
  (:import-from #:ats/algorithms/resume-score
                #:update-score
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
                #:where)
  (:import-from #:common/auth
                #:require-scope
                #:require-role)
  (:import-from #:ats/models/score))
(in-package #:ats/api/score)


(defclass score-and-recommendations ()
  ((score :initarg :score
          :type integer
          :documentation "Соответствие резюме вакансии, в процентах, от 0 до 100.")
   (recommendations :initarg :recommendations
                    :type (soft-list-of string)
                    :documentation "Рекомендации, как улучшить резюме или подтянуть знания.")))

(defmethod print-object ((obj score-and-recommendations) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (score recommendations)
        obj
      (if recommendations
          (format stream "~A~2%~{~A~^~%~}"
                  score
                  recommendations)
          (format stream "~A"
                  score)))))

(defun get-recommendations (job-id user-id)

  (let ((score (when user-id
                 (or
                  (first
                   (mito:select-by-sql 'ats/models/score::score
                                       "
select *
from ats.score as s
join ats.applicant as a on s.applicant_id = a.id
where s.job_id = ?
  and a.user_id = ?
"
                                       :binds (list job-id user-id)))
                  ;; Если не нашли, рассчитаем на-лету
                  (update-score job-id user-id)))))
    (cond
      (score
       (let ((total (min (+ (* (ats/models/score::score-fio-filled score)
                               5)
                            (* (ats/models/score::score-email-filled score)
                               5)
                            (* (ats/models/score::score-phone-filled score)
                               5)
                            (* (ats/models/score::score-telegram-filled score)
                               5)
                            (* (ats/models/score::score-about-filled score)
                               5)
                            (* (ats/models/score::score-experience-filled score)
                               5)
                            ;; Предыдущие пункты в сумме могут дать 30%,
                            ;; а experience-match может быть от 0 до 100,
                            ;; так что его надо сжать до диапазона 0 - 60
                            (coerce (ceiling (* 0.6 (ats/models/score::score-experience-match score)))
                                    'integer))
                         ;; На всякий случай ограничим сверху, чтобы не было нелепых выходов за более 100%
                         100))
             (recommendations
               (remove-if #'null
                          (list (when (zerop (ats/models/score::score-fio-filled score))
                                  "Укажите в резюме имя.")
                                (when (zerop (ats/models/score::score-email-filled score))
                                  "Укажите в резюме email.")
                                (when (zerop (ats/models/score::score-phone-filled score))
                                  "Укажите номер тефона.")
                                (when (zerop (ats/models/score::score-telegram-filled score))
                                  "Укажите логин Telegram.")
                                (when (zerop (ats/models/score::score-about-filled score))
                                  "Заполните раздел \"Обо мне\".")
                                (when (zerop (ats/models/score::score-experience-filled score))
                                  "Опишите свой опыт работы.")
                                 
                                (when (< 50 (ats/models/score::score-experience-match score))
                                  "Поступите в академию Reksoft, чтобы получить знания для соответствия вакансии.")))))
         (make-instance 'score-and-recommendations
                        :score total
                        :recommendations recommendations)))
      ;; Score не может быть посчитан, потому что у пользователя пока нет резюме.
      (t
       (make-instance 'score-and-recommendations
                      :score 0
                      :recommendations (list "Добавьте резюме."
                                             "Опишите опыт работы."))))))


(define-rpc-method (ats-api get-score) (&key job-id)
  (:summary "Отдаёт оценку соответствия кандидата вакансии и рекомендации, как её улучшить.")
  (:param job-id integer "ID вакансии")
  (:result score-and-recommendations)
  (with-connection ()
    (with-session ((user-id) :require nil)
      (get-recommendations job-id user-id))))

