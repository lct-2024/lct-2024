(uiop:define-package #:ats/models/news-post
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
  (:import-from #:ats/models/speciality
                #:speciality)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:timestamp-duration+
                #:duration)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:common/chat
                #:create-new-chat)
  (:import-from #:common/markup
                #:markdown->html))
(in-package #:ats/models/news-post)


(deftable news-post ()
  ((project :initarg :project
            :type (or null project)
            :col-type (or :null project)
            :documentation "Проект к которому привязана джоба."
            :accessor news-post-project)
   (title :initarg :title
          :type string
          :col-type :text
          :accessor news-post-title)
   (image :initarg :image
          :type string
          :col-type :text
          :accessor news-post-image)
   (text :initarg :description
         :type string
         :col-type :text
         :initform ""
         :documentation "Содержимое новости в Markdown формате."
         :accessor news-post-text)
   (html :initarg :html
         :type string
         :col-type :text
         :initform ""
         :documentation "Содержимое новости в HTML формате."
         :accessor news-post-html)
   (chat-id :initarg :chat-id
           :initform nil
           :type (or null string)
           :col-type (or :null :text)
           :documentation "ID чата, привязанного к объекту."
           :accessor news-post-chat-id))
  (:table-name "ats.news_post"))


(defmethod print-object ((news-post news-post) stream)
  (print-unreadable-object (news-post stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id news-post)
            (news-post-title news-post))))


(defmethod mito:insert-dao :after ((obj news-post))
  (setf (news-post-chat-id obj)
        (create-new-chat "news-post"
                         (princ-to-string (mito:object-id obj))
                         :title (news-post-title obj)))
  (setf (news-post-html obj)
        (handler-case
            (markdown->html
             (news-post-text obj))
          (serious-condition ()
            ;; Просто вернём текст если не получилось конвертнуть
            (news-post-text obj))))
  (mito:save-dao obj))
