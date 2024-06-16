(uiop:define-package #:ats/api/analytics
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/models/city
                #:city)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:mito
                #:deftable)
  (:shadow #:step))
(in-package #:ats/api/analytics)


(deftable step ()
  ((num :initarg :num
        :col-type :integer
        :type integer
        :documentation "Номер этапа")
   (title :initarg :title
          :col-type :text
          :type string
          :documentation "Этап воронки найма")
   (candidates-count :initarg :candidates-count
                     :col-type :integer
                     :type integer
                     :documentation "Этап воронки найма")))


(defmethod print-object ((obj step) stream)
  (format stream "~A. ~S: ~A"
          (step-num obj)
          (step-title obj)
          (step-candidates-count obj)))


(define-rpc-method (ats-api get-analytics) ()
  (:summary "Статистику по количеству кандидатов на каждом из этапов воронки найма")
  (:result (serapeum:soft-list-of step))
  (with-connection ()
    (mito:select-by-sql 'step
                        "
select 0 as num
     , 'Ожидают обработки' as title
     , (
         select count(*) from ats.job_applicant
          where application_step_id is NULL
       ) as candidates_count
union all
select num, title, count(*) as candidates_count
from ats.application_step as astep
left join ats.job_applicant as ja on astep.id = ja.application_step_id
group by num, title
order by num
"
                        )))

