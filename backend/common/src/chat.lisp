(uiop:define-package #:common/chat
  (:use #:cl)
  (:import-from #:chat/client
                #:create-chat
                #:chat-id)
  (:export #:create-new-chat))
(in-package #:common/chat)


(defun create-new-chat (content-type content-id)
  (let ((client (chat/client:connect)))
    (chat-id (create-chat client
                          :content-type content-type
                          :content-id content-id))))
