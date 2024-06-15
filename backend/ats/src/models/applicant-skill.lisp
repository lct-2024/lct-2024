(uiop:define-package #:ats/models/applicant-skill
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:select-one-column
                #:sql-fetch-all)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/models/skill
                #:get-skill-ids
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


(-> get-skills-simple (applicant)
    (values (soft-list-of string) &optional))

(defun get-skills-simple (applicant)
  (select-one-column "
select s.title
from ats.skill as s
join ats.applicant_skill as askill on s.id = askill.skill_id
where askill.applicant_id = ? "
                     :binds (list (object-id applicant))
                     :column :title))


(-> update-skills-simple (applicant (soft-list-of string))
    (values applicant &optional))

(defun update-skills-simple (applicant skills)
  (let* ((current-skills
           (mito:retrieve-by-sql "
select askill.id, s.title
from ats.skill as s
join ats.applicant_skill as askill on s.id = askill.skill_id
where askill.applicant_id = ? "
                                 :binds (list (object-id applicant)))))
    (loop for row in current-skills
          for title = (getf row :title)
          for id = (getf row :id)
          for need-to-keep = (member title skills :test #'string=)
          if need-to-keep
          collect title into skill-titles-to-keep
          else collect id into skill-ids-to-remove
          finally (with-transaction
                    (when skill-ids-to-remove
                      (loop for id in skill-ids-to-remove
                            do (mito:execute-sql "delete from ats.applicant_skill where id = ?"
                                                 (list id))))
                    (let* ((skill-titles-to-add
                             (set-difference skills
                                             skill-titles-to-keep
                                             :test #'string-equal))
                           (skill-ids (get-skill-ids skill-titles-to-add)))
                      (bind-applicant-to-skills applicant skill-ids))))
    (values applicant)))
