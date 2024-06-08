(uiop:define-package #:ats/models/skill
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
(in-package #:ats/models/skill)


(deftable skill ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor skill-title))
  (:table-name "ats.skill"))


(defmethod print-object ((skill skill) stream)
  (print-unreadable-object (skill stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id skill)
            (skill-title skill))))


(defun suggest-skills (query)
  (with-connection ()
    (mito:select-by-sql 'skill
                        "
select *
from ats.skill
where title COLLATE \"ru_RU\" ilike ?
order by title COLLATE \"ru_RU\""
                        :binds (list (fmt "%~A%" query)))))


(defun load-all-skills ()
  (with-connection ()
    (loop with data = (list "Agile"
                            "DevOps"
                            "Cloud Computing"
                            "Machine Learning"
                            "Artificial Intelligence"
                            "Cybersecurity"
                            "Data Science"
                            "Big Data"
                            "Blockchain"
                            "IoT"
                            "Mobile Development"
                            "Web Development"
                            "UI/UX Design"
                            "Networking"
                            "Virtualization"
                            "Containers"
                            "Linux Administration"
                            "Windows Administration"
                            "Automation"
                            "Testing/QA"
                            "Database Management"
                            "Project Management"
                            "Technical Writing"
                            "Problem Solving"
                            "Critical Thinking"
                            "Teamwork"
                            "Communication Skills"
                            "Гибкая методология разработки"
                            "Облачные вычисления"
                            "Машинное обучение"
                            "Искусственный интеллект"
                            "Кибербезопасность"
                            "Анализ данных"
                            "Большие данные"
                            "Цифровые технологии блокчейн"
                            "Интернет вещей"
                            "Разработка мобильных приложений"
                            "Веб-разработка"
                            "Дизайн пользовательского интерфейса"
                            "Сетевые технологии"
                            "Виртуализация"
                            "Контейнеры"
                            "Администрирование Linux"
                            "Администрирование Windows"
                            "Автоматизация"
                            "Тестирование и обеспечение качества"
                            "Управление базами данных"
                            "Управление проектами"
                            "Техническое письмо"
                            "Решение проблем"
                            "Критическое мышление"
                            "Командная работа"
                            "Коммуникативные навыки")
          for title in (remove-duplicates
                        (sort data
                              #'string<)
                        :test #'string-equal)
          do (create-dao 'skill
                         :title (trim title)))))
