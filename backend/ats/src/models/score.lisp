(uiop:define-package #:ats/models/score
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:ats/models/job
                #:job)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:import-from #:ats/models/application-step
                #:application-step)
  (:export
   #:bind-job-to-applicants
   #:get-scores))
(in-package #:ats/models/score)


(deftable score ()
  ((job :initarg :job
        :type job
        :col-type job
        :accessor score-job)
   (applicant :initarg :applicant
              :type applicant
              :col-type applicant
              :accessor score-applicant)
   (fio-filled :initarg :fio-filled
               :fio-filled integer
               :col-type :integer
               :initform 0
               :documentation "Имя заполнено"
               :accessor score-fio-filled)
   (email-filled :initarg :email-filled
                 :email-filled integer
                 :col-type :integer
                 :initform 0
                 :documentation "Email заполнено"
                 :accessor score-email-filled)
   (phone-filled :initarg :phone-filled
                 :phone-filled integer
                 :col-type :integer
                 :initform 0
                 :documentation "Телефон заполнено"
                 :accessor score-phone-filled)
   (telegram-filled :initarg :telegram-filled
                    :telegram-filled integer
                    :col-type :integer
                    :initform 0
                    :documentation "Telegram заполнено"
                    :accessor score-telegram-filled)
   (about-filled :initarg :about-filled
                 :about-filled integer
                 :col-type :integer
                 :initform 0
                 :documentation "About заполнено"
                 :accessor score-about-filled)
   (experience-filled :initarg :experience-filled
                      :experience-filled integer
                      :col-type :integer
                      :initform 0
                      :documentation "Experience заполнено"
                      :accessor score-experience-filled)
   (experience-match :initarg :experience-match
                     :experience-match integer
                     :col-type :integer
                     :initform 0
                     :documentation "Experience заполнено"
                     :accessor score-experience-match))
  (:documentation "Хранит метрики соответствия кандидата вакансии.")
  (:table-name "ats.score"))


(defmethod print-object ((obj score) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A JOB=~A APPLICANT=~A"
            (object-id obj)
            (score-job obj)
            (score-applicant obj))))

