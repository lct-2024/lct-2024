(uiop:define-package #:ats/api/applicants
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:ats/models/education
                #:education)
  (:import-from #:mito
                #:find-dao
                #:select-dao)
  (:import-from #:sxql
                #:where
                #:order-by))
(in-package #:ats/api/applicants)


(define-rpc-method (ats-api get-applicants) ()
  (:summary "Отдаёт всех кандидатов")
  (:result (serapeum:soft-list-of applicant))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (select-dao 'applicant)))))


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


(define-rpc-method (ats-api get-applicant-education) (applicant-id)
  (:summary "Отдаёт записи об образовании кандидата")
  (:param applicant-id integer "ID кандидата из списка отдаваемого get-applicants")
  (:result (serapeum:soft-list-of education))
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (values
       (select-dao 'education
         (where (:= :id applicant-id))
         (order-by :from))))))


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
                        :from (local-time:parse-timestring from)
                        :to (when to
                              (local-time:parse-timestring to))
                        :type type
                        :speciality-id speciality-id)))))


(define-rpc-method (ats-api delete-applicant-education) (education-id)
  (:summary "Добавляет новую запись об образовании кандидата")
  (:param education-id integer "ID записи об образовании.")
  (:result nil)
  (with-connection ()
    (with-session (user-id)
      (declare (ignore user-id))
      (mito:execute-sql "delete from ats.education where id = ?"
                        (list education-id))
      (values nil))))


