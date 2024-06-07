(uiop:define-package #:ats/models/city
  (:use #:cl)
  (:import-from #:dex)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:create-dao
                #:insert-dao
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:str
                #:split
                #:trim))
(in-package #:ats/models/city)


(deftable city ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor city-title)
   ;; (lowercased-title :initarg :lowercased-title
   ;;                   :type string
   ;;                   :col-type :text
   ;;                   :documentation "Название, приведённое к нижнему регистру, чтобы делать регистро")
   )
  (:table-name "ats.city"))


(defmethod print-object ((city city) stream)
  (print-unreadable-object (city stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id city)
            (city-title city))))


(defun suggest-cities (query)
  (with-connection ()
    (mito:select-by-sql 'city
                        "
select *
from ats.city
where title COLLATE \"ru_RU\" ilike ?"
                        :binds (list (fmt "%~A%" query)))))


(defun load-all-cities ()
  ;; https://gist.github.com/helart/96225136a784f8a3987398be96456dce
  (with-connection ()
    (loop with data = (dex:get "https://gist.githubusercontent.com/helart/96225136a784f8a3987398be96456dce/raw/8d4b63baf056ca0680c6fc18fc76f17c83525c28/txt-cities-russia.txt")
          for title in (split #\Newline data :omit-nulls t)
          do (create-dao 'city
                         :title (trim title)))))
