(uiop:define-package #:common/permissions
  (:use #:cl)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:openrpc-server
                #:return-error)
  (:import-from #:serapeum
                #:fmt)
  (:export #:assert-can-modify
           #:scope-for-editing
           #:scope-for-deleting
           #:assert-can-delete))
(in-package #:common/permissions)


(defgeneric scope-for-editing (object)
  (:documentation "Возвращает строку с именем роли которую нужно иметь, чтобы редактировать объект или NIL, если такой роли нет.")
  
  (:method ((object t))
    nil))


(defgeneric scope-for-deleting (object)
  (:documentation "Возвращает строку с именем роли которую нужно иметь, чтобы удалить объект или NIL, если такой роли нет.")
  
  (:method ((object t))
    nil))


(defgeneric assert-can-modify (user-id object scopes)
  (:documentation "Если пользователь с USER-ID не админ и не владелец объекта, то выполнение метода будет прервано
и API вернёт ошибку.

Методы должны возвращать T, если пользователь может изменять объект. А :around метод уже вернёт ошибку.")
  
  (:method ((user-id integer) (object t) scopes)
    "Если нужной роли нет, то доступ запрещён. Но метод можно переопрелить и например проверять, что user-id - создатель объекта."
    (let ((required-scope (scope-for-editing object)))
      (when (and required-scope
                 (member required-scope scopes
                         :test #'string-equal))
        (values t))))
  
  (:method :around ((user-id integer) (object t) scopes)
    (unless (call-next-method)
      (let ((required-scope (scope-for-editing object)))
        (if required-scope
            (return-error (fmt "User with id = ~A can't change the object ~A. Required scope: ~A"
                               user-id object required-scope))
            (return-error (fmt "User with id = ~A can't change the object ~A."
                               user-id object)))))))


(defgeneric assert-can-delete (user-id object scopes)
  (:documentation "Если пользователь с USER-ID не админ и не владелец объекта, то выполнение метода будет прервано
и API вернёт ошибку.

Методы должны возвращать T, если пользователь может удалять объект. А :around метод уже вернёт ошибку.")
  
  (:method ((user-id integer) (object t) scopes)
    "Если нужной роли нет, то доступ запрещён. Но метод можно переопрелить и например проверять, что user-id - создатель объекта."
    (let ((required-scope (scope-for-deleting object)))
      (when (and required-scope
                 (member required-scope scopes
                         :test #'string-equal))
        (values t))))

  (:method :around ((user-id integer) (object t) scopes)
    (unless (call-next-method)
      (let ((required-scope (scope-for-deleting object)))
        (if required-scope
            (return-error (fmt "User with id = ~A can't delete the object ~A. Required scope: ~A"
                               user-id object required-scope))
            (return-error (fmt "User with id = ~A can't delete the object ~A."
                               user-id object)))))))
