(uiop:define-package #:ats/models/project-theme
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
  (:import-from #:ats/models/project
                #:project)
  (:import-from #:ats/models/theme
                #:theme)
  (:import-from #:40ants-pg/transactions
                #:with-transaction)
  (:export #:bind-project-to-themes
           #:get-project-themes))
(in-package #:ats/models/project-theme)


(deftable project-theme ()
  ((project :initarg :project
            :type project
            :col-type project
            :accessor project)
   (theme :initarg :theme
          :type theme
          :col-type theme
          :accessor theme))
  (:table-name "ats.project_theme"))


(defmethod print-object ((obj project-theme) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~A PROJECT=~A THEME=~A"
            (object-id obj)
            (project obj)
            (theme obj))))



(defun bind-project-to-themes (project theme-ids)
  (with-transaction
      (loop for theme-id in theme-ids
            for link = (mito:create-dao 'project-theme
                                        :project project
                                        :theme-id theme-id)
            collect (theme link))))


(defun get-project-themes (project)
  (mito:select-by-sql 'theme
                      "
select theme.*
from ats.theme
join ats.project_theme on theme.id = project_theme.theme_id
where project_theme.project_id = ?"
                      :binds (list (mito:object-id project))))
