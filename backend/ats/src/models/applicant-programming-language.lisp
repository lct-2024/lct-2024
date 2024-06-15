(uiop:define-package #:ats/models/applicant-programming-language
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
  (:import-from #:ats/models/programming-language
                #:programming-language)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:export
   #:bind-applicant-to-programming-languages))
(in-package #:ats/models/applicant-programming-language)


(deftable applicant-programming-language ()
  ((applicant :initarg :applicant
            :type applicant
            :col-type applicant
            :accessor applicant)
   (programming-language :initarg :programming-language
                         :type programming-language
                         :col-type programming-language
                         :accessor programming-language))
  (:table-name "ats.applicant_programming_language"))


(defmethod print-object ((obj applicant-programming-language) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A APPLICANT=~A LANG=~A"
            (object-id obj)
            (applicant obj)
            (programming-language obj))))


(defun bind-applicant-to-programming-languages (applicant programming-language-ids)
  (with-transaction
      (loop for programming-language-id in programming-language-ids
            for link = (mito:create-dao 'applicant-programming-language
                                        :applicant applicant
                                        :programming-language-id programming-language-id)
            collect (programming-language link))))
