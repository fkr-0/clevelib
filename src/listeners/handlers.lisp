(defpackage :clevelib.handlers
  (:use :cl :clevelib.synchronization)
  (:export :event-handler
    :make-event-handler
    :register-handler
    :unregister-handler
    
    :call-handler
    :find-handlers))
(in-package :clevelib.handlers)

(defstruct event-handler
  (event-type nil :type symbol)
  (callback nil :type function)
  (target nil)
  (options (make-hash-table) :type hash-table))

;; (defun make-event-handler (event-type callback &key target options)
;;   (make-instance 'event-handler
;;     :event-type event-type
;;     :callback callback
;;     :target target
;;     :options (alexandria:plist-hash-table options)))

(defvar *handlers* (make-hash-table :test 'equal))

(defun clear-handlers ()
  "Clears all handlers."
  (clrhash *handlers*)
  )

(defun register-handler (handler)
  (let ((key (event-handler-event-type handler)))
    (setf (gethash key *handlers*)
      (cons handler (gethash key *handlers*)))))

(defun unregister-handler (handler)
  (let ((key (event-handler-event-type handler)))
    (setf (gethash key *handlers*)
      (remove handler (gethash key *handlers*) :test #'equal))))

(defun call-handler (handler event)
  "Calls the callback function of the handler with the event as argument."
  (format t "Calling handler ~a with event ~a~%" handler event)
  (let ((callback (gethash handler *handlers*))) ;; Cache lookup
    (funcall callback event)))

(defun find-handlers (event-type &key target)
  (let ((handlers (gethash event-type *handlers*))) ;; Cache lookup
    (remove-if-not (lambda (handler)
                     (and (eq (event-handler-event-type handler) event-type)
                       (or (null target)
                         (eq (event-handler-target handler) target))))
      handlers)))
