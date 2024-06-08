(uiop:define-package #:ats/models/applicant-skill
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
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/models/skill
                #:skill)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:export
   #:bind-applicant-to-skills))
(in-package #:ats/models/applicant-skill)


(deftable applicant-skill ()
  ((applicant :initarg :applicant
            :type applicant
            :col-type applicant
            :accessor applicant)
   (skill :initarg :skill
                         :type skill
                         :col-type skill
                         :accessor skill))
  (:table-name "ats.applicant_skill"))


(defmethod print-object ((obj applicant-skill) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A APPLICANT=~A LANG=~A"
            (object-id obj)
            (applicant obj)
            (skill obj))))



(defun bind-applicant-to-skills (applicant skill-ids)
  (with-transaction
      (loop for skill-id in skill-ids
            for link = (mito:create-dao 'applicant-skill
                                        :applicant applicant
                                        :skill-id skill-id)
            collect (skill link))))
