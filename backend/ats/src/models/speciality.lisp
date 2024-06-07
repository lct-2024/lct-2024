(uiop:define-package #:ats/models/speciality
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
(in-package #:ats/models/speciality)


(deftable speciality ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor speciality-title))
  (:table-name "ats.speciality"))


(defmethod print-object ((speciality speciality) stream)
  (print-unreadable-object (speciality stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id speciality)
            (speciality-title speciality))))


(defun suggest-specialities (query)
  (with-connection ()
    (mito:select-by-sql 'speciality
                        "
select *
from ats.speciality
where title COLLATE \"ru_RU\" ilike ?
order by title COLLATE \"ru_RU\""
                        :binds (list (fmt "%~A%" query)))))


(defun load-some-specialities ()
  (with-connection ()
    (loop with data = '("Программист"
                        "Системный администратор"
                        "Аналитик данных"
                        "Инженер по разработке программного обеспечения"
                        "QA инженер (тестировщик)"
                        "Специалист по информационной безопасности"
                        "Сетевой инженер"
                        "UX/UI дизайнер"
                        "Big Data специалист"
                        "DevOps инженер"
                        "Инженер по машинному обучению и искусственному интеллекту"
                        "Бизнес-аналитик"
                        "Специалист по работе с облачными технологиями"
                        "Администратор баз данных"
                        "Специалист по тестированию безопасности"
                        "Инженер по автоматизации процессов"
                        "Специалист по интернет-маркетингу"
                        "Web-разработчик"
                        "Инженер по робототехнике"
                        "Специалист по блокчейн технологиям")
          for title in data
          do (create-dao 'speciality
                         :title (trim title)))))
