(in-package :cl-user)

(declaim (optimize (debug 3) (safety 3)))

(when (probe-file ".local-config.lisp")
  (load ".local-config.lisp"))

(ql:quickload '(passport))

;; Это я использовал в 2023
;; но до так и не довёл до состояния, когда расширение
;; можно использовать на любой странице сервиса:
;; (when (probe-file "~/projects/sly-reblocks.lisp")
;;   (load "~/projects/sly-reblocks.lisp"))


(defun start-all ()
  "Запускает все сервисы в режиме разработки."
  (passport:start))


(defun stop-all ()
  "Останавливает все сервисы."
  (passport:stop))
