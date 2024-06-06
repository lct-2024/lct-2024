(uiop:define-package #:chat/chat/api
  (:use #:cl
        #:common/utils)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/model
                #:chat-archived-p
                #:chat)
  (:import-from #:chat/chat-member/model
                #:chat-member)
  (:import-from #:chat/chat-team/model
                #:chat-team)
  (:import-from #:mito
                #:object-id
                #:create-dao)
  (:import-from #:serapeum
                #:fmt))
(in-package #:chat/chat/api)


(define-rpc-method (chat-api create-chat) (&key private title content-type content-id)
  (:summary "Создаёт новый чат для команды.")
  (:description "Если private True, то в чат смогут писать не только члены команды, но и кто угодно.")
  (:param private boolean
          "Если выставить в True, то в чат смогут писать только члены команды.")
  (:param title string)
  (:param content-type (or null string))
  (:param content-id (or null string))
  (:result chat)

  (when (and (or content-id content-type)
             (not (and content-id content-type)))
    (openrpc-server:return-error "Both content-type and content-id should be specified."
                                 :code 1001))

  (with-connection ()
    (let ((chat (when (and content-id content-type)
                  (find-dao 'chat
                            :content-type content-type
                            :content-id content-id))))
      (cond
        (chat
         ;; Return existing chat if it is aready created for the given content-id
         (values chat))
        (t
         (let* ((chat (create-dao 'chat :id (make-uuid)
                                        :private private
                                        :title title
                                        :content-type content-type
                                        :content-id content-id)))
           (values chat)))))))


(define-rpc-method (chat-api get-chat) (id)
  (:summary "Запрашивает данные о чате.")
  (:description "Если чат не найден, то возвращает ошибку.")
  (:param id string)
  (:result chat)

  (handler-bind
      ((error (lambda (err)
                (unless (typep err 'jsonrpc:jsonrpc-error)
                  (openrpc-server:return-error (fmt "Ошибка: ~S" err)
                                               :code 1000)))))
    ;; Провалидируем что id это корректный uuid
    (uuid:make-uuid-from-string id)

    (with-connection ()
      (let ((chat (find-dao 'chat :id id)))
        (cond
          (chat chat)
          (t
           (openrpc-server:return-error (fmt "Чат с id ~S не найден." id)
                                        :code 1003)))))))

(define-rpc-method (chat-api archive-chat) (id)
  (:summary "Архивирует чат.")
  (:description "Если чат не найден, то возвращает ошибку.")
  (:param id string)
  (:result chat)

  (handler-bind
      ((error (lambda (err)
                (unless (typep err 'jsonrpc:jsonrpc-error)
                  (openrpc-server:return-error (fmt "Ошибка: ~S" err)
                                               :code 1000)))))
    ;; Провалидируем что id это корректный uuid
    (uuid:make-uuid-from-string id)

    (with-connection ()
      (let ((chat (find-dao 'chat :id id)))
        (cond
          (chat
           (setf (chat-archived-p chat)
                 t)
           (mito:save-dao chat)
           chat)
          (t
           (openrpc-server:return-error (fmt "Чат с id ~S не найден." id)
                                        :code 1003)))))))


(define-rpc-method (chat-api get-all-chats) (content-type)
  (:summary "Отдаёт список чатов с заданным content-type.")
  (:description "Этот метод нужен чтобы в KinoJaba получить все чаты и добавить рандомные сообщения.")
  (:param content-type string)
  (:result (list-of chat))

  (with-connection ()
    (mito:retrieve-dao 'chat :content-type content-type)))
