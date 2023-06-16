;; (defpackage :clevelib.dispatcher
;;   (:use :cl  :clevelib.handlers :clevelib.synchronization :bordeaux-threads)
;;   (:documentation "Event dispatcher package")
;;   (:export :dispatch-event
;;     :async-exec
;;     :run-event-loop
;;     :*event-dispatch-condition*))
;; (in-package :clevelib.dispatcher)

;; (defvar *event-dispatch-condition* (clevelib.synchronization:make-event-condition-variable))



;; (defun dispatch-event (event target )
;;   (let* (

;;           (handlers (clevelib.handlers:find-handlers event :target target)))
;;     (dolist (handler handlers)
;;       (clevelib.handlers:call-handler event handler )))
;;   (clevelib.synchronization:event-condition-variable-notify *event-dispatch-condition*))

;; ;; (defun process-events (&optional timeout)
;; ;;   (loop
;; ;;     (let ((event (dequeue-event)))
;; ;;       (when event
;; ;;         (dispatch-event event)
;; ;;         (when timeout (sleep (/ timeout 1000.0)))))))

;; (defun async-exec (function &rest args)
;;   (bordeaux-threads:make-thread (lambda () (apply function args))))

;; ;; (defun run-event-loop (&key (timeout 100))
;; ;;   (loop (process-events timeout)))
