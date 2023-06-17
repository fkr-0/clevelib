;;;; error-handling.lisp -- error handling for the event system
;;;; This module handles error handling during event processing and asynchronous
;;;; task execution.
;;;; during event processing and asynchronous task execution. It defines a global
;;;; error handler, a default implementation for the error handler, a macro to
;;;; set a custom error handler, and a function to handle errors using the
;;;; current error handler. This design allows for easy customization of error
;;;; handling behavior and supports the overall event system structure.



(defpackage :clevelib.errors
  (:use :cl)
  (:export
    #:*event-error-handler*
    #:with-event-error-handler
    #:default-event-error-handler))

(in-package :clevelib.errors)

(defvar *event-error-handler* #'default-event-error-handler
  "The global event error handler.
This function is called with two arguments: the error object and the context.
The context is an alist containing additional information related to the error.")

(defun default-event-error-handler (error context)
  "The default event error handler.
This function logs the error message and the context, and continues event processing."
  (declare (ignorable context))
  (format t "Event error: ~a~%" error))

(defmacro with-event-error-handler ((handler) &body body)
  "Execute BODY with the specified event error HANDLER."
  `(let ((*event-error-handler* ,handler))
     ,@body))
(defun default-event-error-handler (error context)
  (format t "Event error: ~a~%Context: ~a~%" error context))

(defun handle-event-error (error context)
  "Handle an event error using the current event error handler."
  (funcall *event-error-handler* error context))
(define-condition event-dispatch-error (error)
  ((event :initarg :event :reader event-dispatch-error-event)))

(defun default-event-error-handler (error context)
  (typecase error
    (event-dispatch-error
      (format t "Error dispatching event: ~a~%" (event-dispatch-error-event error)))
    (otherwise
      (format t "Generic event error: ~a~%" error))))


(defun handle-event-error (event error) "Handle errors occurring during event processing." (format t "Error occurred while processing event ~A: ~A~%" event error) (finish-output) ;; Log the error
  (log-event-error event error) ;; Determine if the event should be retried
  (if (should-retry-event-p event) (progn (format t "Retrying event ~A...~%" event) (finish-output) ;; Retry the event
                                     (process-event event)) (format t "Skipping retry of event ~A.~%" event) (finish-output)))
(defun log-event-error (event error) "Log an error occurring during event processing." ;; Log to file
  (with-open-file (stream "event-errors.log" :direction :output :if-exists :append :if-does-not-exist :create) (format stream "A: Error ~A occurred while processing event ~A.%" (get-universal-time) error event)) ;; Log to database
  (log-to-database :event-error event error))

(defun should-retry-event-p (event)
  "Determine if the given event should be retried after an error."
  (case (event-type event)
    (:high-priority t) (:normal-priority
                         (< (random 1.0) 0.5)) (:low-priority nil)))







;; Define higher level error handlers
(defun log-and-continue-handler (error context)
  "Log the error and continue processing."
  (format t "Event error: ~a~%Context: ~a~%" error context))

(defun abort-handler (error context)
  "Abort event processing."
  (declare (ignorable context))
  (format t "Aborting due to error: ~a~%" error)
  (return-from dispatch-event))

;; Examples of using the error handlers
(with-event-error-handler (log-and-continue-handler)
  (dispatch-event some-event))

(with-event-error-handler (abort-handler)
  (dispatch-event other-event))

;; Handle specific event dispatch errors
(defun dispatch-event (event)
  (handler-case
    (process-event event)
    (event-dispatch-error (e)
      (handle-event-error e (list :event event)))))

;; Example event dispatch error
(define-condition invalid-event-data (event-dispatch-error)
  ((invalid-data :initarg :invalid-data :reader invalid-event-data)))


(defun process-event (event)
  (when (invalid-event-data event)
    (error 'invalid-event-data :invalid-data "some bad data")))

;; The error handler will catch the invalid-event-data condition
;; and handle it specifically.
(dispatch-event some-event)

;; Output:
;; Error dispatching event: some-event
;; Context: (:event some-event)
