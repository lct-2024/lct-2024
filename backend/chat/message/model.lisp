(uiop:define-package #:chat/message/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id))
(in-package #:chat/message/model)


(defclass message ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (chat-id :initarg :chat-id
            :type string
            :col-type :uuid
            :reader chat-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :bigint
            :reader user-id
            :documentation "ID пользователя, который написал сообщение.")
   (message :initarg :message
            :type string
            :col-type :text
            :reader chat-message
            :documentation "Текст сообщения"))
  (:documentation "Одно сообщение из чата.")
  (:table-name "chat.message")
  (:metaclass dao-table-class))


(defclass chat-info-mixin ()
  ((chat-title :initarg :chat-title
               :type (or null string)
               :col-type (or :null :text)
               :accessor chat-title)
   (content-type :initarg :chat-content-type
                 :type (or null string)
                 :col-type (or :null :text)
                 :accessor chat-content-type
                 :documentation "Тип контента, для которого создан чат, например \"blog-page\".")
   (content-id :initarg :chat-content-id
               :type (or null string)
               :col-type (or :null :text)
               :accessor chat-content-id
               :documentation "ID контента, для которого создан чат, например URL страницы."))
  (:metaclass mito:dao-table-mixin))


(defclass message-with-base-chat-info (message chat-info-mixin)
  ()
  (:documentation "Одно сообщение из чата + дополнительная информация о чате.")
  (:table-name "chat.message")
  (:metaclass dao-table-class))

