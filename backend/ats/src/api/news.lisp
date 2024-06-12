(uiop:define-package #:ats/api/news
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:40ants-openrpc/jwt
                #:with-session)
  (:import-from #:ats/api
                #:ats-api)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:mito
                #:find-dao
                #:select-dao
                #:includes
                #:deftable)
  (:import-from #:ats/algorithms/resume-score
                #:calculate-resume-score)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:duration
                #:timestamp-duration+)
  (:import-from #:local-time-duration
                #:duration)
  (:import-from #:sxql
                #:order-by
                #:where)
  (:import-from #:common/auth
                #:require-scope
                #:require-role)
  (:import-from #:ats/models/news-post
                #:news-post)
  (:import-from #:ats/models/project
                #:project)
  (:import-from #:common/rpc
                #:define-delete-method
                #:define-update-method)
  (:import-from #:common/permissions
                #:scope-for-deleting
                #:scope-for-editing))
(in-package #:ats/api/news)


(define-rpc-method (ats-api get-news) (&key project-id)
  (:summary "Отдаёт все новостные посты, или посты привязанные к проекту с указанным ID.")
  (:param project-id integer "ID проекта новости которого надо отдать.")
  (:result (serapeum:soft-list-of news-post))
  (with-connection ()
    (values
     (select-dao 'news-post
       (includes 'project)
       (order-by :title)
       (when project-id
         (where (:= :project-id project-id)))))))


(define-rpc-method (ats-api create-news-post) (title text 
                                                     &key
                                                     project-id)
  (:summary "Добавляет в базу новую вакансию")
  (:param title string "Название вакансии")
  (:param text string "Описание вакансии")
  (:param project-id integer "ID проекта к которому относится новость. Можно получить методом get_projects.")
  (:result news-post)
  
  (with-connection ()
    (with-session ((user-id scopes))
      (require-scope user-id scopes "ats.news-post.create" "create a news-post")
      
      (mito:create-dao 'news-post
                       :title title
                       :text text
                       :project-id project-id))))


(define-update-method (ats-api update-news-post news-post)
                      (id title text)
  "Обновить новость."
  (find-dao 'news-post
            :id id))


(define-delete-method (ats-api delete-news-post news-post)
  "Удалить новость.")


(defmethod scope-for-editing ((obj news-post))
  "ats.news-post.edit")

(defmethod scope-for-deleting ((obj news-post))
  "ats.news-post.delete")
