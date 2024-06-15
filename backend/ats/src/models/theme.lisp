(uiop:define-package #:ats/models/theme
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
                #:trim)
  (:export
   #:get-theme-by-id))
(in-package #:ats/models/theme)


(deftable theme ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor theme-title))
  (:table-name "ats.theme"))


(defmethod print-object ((theme theme) stream)
  (print-unreadable-object (theme stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id theme)
            (theme-title theme))))


(defun suggest-themes (query)
  (with-connection ()
    (mito:select-by-sql 'theme
                        "
select *
from ats.theme
where title COLLATE \"ru_RU\" ilike ?
order by title COLLATE \"ru_RU\""
                        :binds (list (fmt "%~A%" query)))))


(defun load-some-themes ()
  (with-connection ()
    (loop with data = '("Финтех"
                        "Госсектор"
                        "IT"
                        "Медиа"
                        "Нефть и газ"
                        "Ретейл"
                        "Коммуникации"
                        "Транспорт"
                        "Другое")
          for title in data
          do (create-dao 'theme
                         :title (trim title)))))


(defun get-theme-by-id (id)
  (mito:find-dao 'theme
                 :id id))


(defun get-theme-id (title)
  (mito:object-id
   (or (first (mito:select-by-sql 'theme
                                  "select * from ats.theme where title collate \"ru_RU\" ilike ?"
                                  :binds (list title)))
       (mito:create-dao 'theme
                        :title title))))

