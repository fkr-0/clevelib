(defpackage :clevelib.synchronization
  (:use :cl :bt)
  (:export :event-mutex
    :with-event-mutex
    :make-event-condition-variable
    :make-event-mutex
    :event-mutex-mutex
    :event-condition-variable
    :event-condition-variable-wait
    :event-condition-variable-notify))

(in-package :clevelib.synchronization)

(defun make-event-mutex ()
  (make-instance 'clevelib.synchronization:event-mutex))

(defstruct event-mutex
  (mutex (bt:make-lock) :type bt:lock))



(defmacro with-event-mutex (mutex &body body)
  `(with-lock-held ((event-mutex-mutex ,mutex))
     ,@body))

;; (defstruct event-condition-variable
;;   (condition-variable nil ))

;; (defmethod setup-event-condition-variable ((condition-variable event-condition-variable))
;;   (setf (event-condition-variable-condition-variable condition-variable)
;;     (bt:make-condition-variable)))

;; (defun event-condition-variable-wait (condition-variable mutex &optional timeout)
;;   (bt:condition-wait (event-condition-variable-condition-variable condition-variable)
;;     (event-mutex-mutex mutex)
;;     :timeout timeout))

;; (defun event-condition-variable-notify (condition-variable)
;;   (bt:condition-notify (event-condition-variable-condition-variable condition-variable)))

(defstruct event-condition-variable
  (waitqueue (sb-thread:make-waitqueue) :read-only t)
  (lock (sb-thread:make-mutex) :read-only t))

(defun event-condition-variable-wait (cv)
  (sb-thread:with-mutex ((event-condition-variable-lock cv))
    (sb-thread:condition-wait (event-condition-variable-waitqueue cv) (event-condition-variable-lock cv))))

(defun event-condition-variable-notify (cv)
  (sb-thread:with-mutex ((my-condition-variable-lock cv))
    (sb-thread:condition-notify (my-condition-variable-waitqueue cv))))
