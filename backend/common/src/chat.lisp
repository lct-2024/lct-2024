(uiop:define-package #:common/chat
  (:use #:cl)
  (:import-from #:chat/client
                #:create-chat
                #:chat-id)
  (:export #:create-new-chat))
(in-package #:common/chat)


(defun create-new-chat (content-type content-id &key title)
  (let ((client (chat/client:connect)))
    (chat-id (create-chat client
                          :content-type content-type
                          :content-id content-id
                          :title title))))


(defun post-to-chat (chat-id message &key (token (uiop:getenv "NOTIFIER_TOKEN")))
  (let ((client (chat/client:connect token)))
    (chat/client:message-id
     (chat/client:post client
                       chat-id
                       message))))
