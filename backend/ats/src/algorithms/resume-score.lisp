(uiop:define-package #:ats/algorithms/resume-score
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:href
                #:dict)
  (:import-from #:str)
  (:import-from #:alexandria
                #:ensure-gethash)
  (:import-from #:ats/models/job
                #:job)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/models/score
                #:score-total
                #:score)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:ats/ai/cv
                #:score-applicant)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:40ants-pg/query
                #:select-one-column)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:mito
                #:object-id))
(in-package #:ats/algorithms/resume-score)


(defvar *scores* (dict))

(defun calculate-resume-score (job applicant)
  "TODO: тут надо сделать реальный алгоритм, а пока пусть так."
  (let ((real-score (mito:find-dao 'score
                                   :job job
                                   :applicant applicant)))
    (cond
      (real-score
       (score-total real-score))
      (t
       (let ((job-scores (ensure-gethash (object-id job)
                                         *scores*
                                         (dict))))
         (ensure-gethash (object-id applicant)
                         job-scores
                         (random 101)))))))



(defun update-score (job-id user-id)
  (with-connection ()
    (let* ((job (mito:find-dao 'job
                               :id job-id))
           (applicant (mito:find-dao 'applicant
                                     :user-id user-id)))
      (when applicant
        (let* ((applicant-id (mito:object-id applicant))
               (score (or (mito:find-dao 'score
                                         :job-id job-id
                                         :applicant-id applicant-id)
                          (mito:create-dao 'score
                                           :job-id job-id
                                           :applicant-id applicant-id))))
          (flet ((filled (value)
                   (or (and value
                            (not (str:emptyp (str:trim value)))
                            1)
                       0))
                 (contact-filled (contacts type)
                   (or
                    (typecase contacts
                      (list
                       (loop for item in contacts
                             thereis (and (typep item 'hash-table)
                                          (gethash type item)
                                          1))))
                    0)))
            (setf (ats/models/score::score-fio-filled score)
                  (filled
                   (ats/models/applicant::applicant-name applicant)))
            (setf (ats/models/score::score-email-filled score)
                  (filled
                   (ats/models/applicant::applicant-email applicant)))
            (setf (ats/models/score::score-phone-filled score)
                  (contact-filled
                   (ats/models/applicant::applicant-contacts applicant)
                   "phone"))
            (setf (ats/models/score::score-phone-filled score)
                  (contact-filled
                   (ats/models/applicant::applicant-contacts applicant)
                   "telegram"))
            (setf (ats/models/score::score-about-filled score)
                  (filled
                   (ats/models/applicant::applicant-about applicant)))
            (setf (ats/models/score::score-experience-filled score)
                  (filled
                   (ats/models/applicant::applicant-experience applicant)))

            (when (or (ats/models/score::score-about-filled score)
                      (ats/models/score::score-experience-filled score))
              ;; TODO: тут надо что-то решить с лимитом запросов
              ;; но пока защитиммся тем, что не будем пересчитывать
              ;; эту часть score, если она уже посчитана.
              ;; И если случается ошибка, то просто залоггируем её,
              ;; чтобы не блокировать обновление остальных score
              (handler-case
                  (with-log-unhandled ()
                    (when (zerop (ats/models/score::score-experience-match score))
                      (setf (ats/models/score::score-experience-match score)
                            (score-applicant
                             (ats/models/job::job-description job)
                             (fmt "~A~3%~A"
                                  (or (ats/models/applicant::applicant-about applicant)
                                      "")
                                  (or (ats/models/applicant::applicant-experience applicant)
                                      ""))))))
                (serious-condition ()
                  nil)))

            (setf (score-total score)
                  (min (+ (* (ats/models/score::score-fio-filled score)
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
            (mito:save-dao score)
            (values)))))))


(defun update-user-scores-in-thread (user-id)
  (flet ((score-updater()
           ;; Небольшая задержка, чтобы закрылась транзакция
           ;; в которой создавался или обновлялся applicant.
           (with-log-unhandled ()
             (sleep 5)
             (let ((job-ids
                     (with-connection ()
                       (select-one-column
                        "
select job_id as id
from ats.score as s
join ats.applicant as a on s.applicant_id = a.id
where a.user_id = ?
"
                        :binds (list user-id)))))
               (loop for job-id in job-ids
                     do (update-score job-id user-id))))))
    (bt2:make-thread #'score-updater
                     :name (fmt "Score update for user-id ~A"
                                user-id))))


(defun score-all-users ()
  ;; Пробовал получить score но даже на небольших тестовых данных что у нас
  ;; есть, это получается порядка 2500р за обработку всех комбинаций.
  ;; так что лучше "скорить" по запросу.
  (flet ((get-ids ()
           (with-connection ()
             (values (select-one-column "select id from ats.job")
                     (select-one-column "select user_id as id from ats.applicant")))))
    (multiple-value-bind (job-ids user-ids)
        (get-ids)
      (let ((num-processed 0))
        (with-simple-restart (skip-the-rest "Skips this combination and all the rest.")
          (loop for job-id in job-ids
                do (loop for user-id in user-ids
                         do (with-simple-restart (skip "Skip score update for job ~A and user ~A"
                                                       job-id user-id)
                              (update-score job-id user-id)
                              (incf num-processed)))))
        (values num-processed)))))
