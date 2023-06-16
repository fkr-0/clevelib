;; prolly not part of the spec
;; event.lisp
;; event.lisp
;;;; clevelib/events.lisp
;;;; This module handles event creation, manipulation, and dispatching.
;; (defpackage :clevelib.core
;;   (:use :cl :clevelib.synchronization :clevelib.dispatcher :clevelib.handlers ) ;:clevelib.utils)
;;   (:export :event
;;     :make-event
;;     :dispatch-event
;;     :add-event-listener
;;     :trigger-event
;;     :event-listener
;;     :event-listener-event
;;     :event-listener-target
;;     :event-listener-callback
;;     :event-listener-filter
;;     :event-listener-delegate
;;     :event-type
;;     :event-target
;;     :event-timestamp
;;     :event-data
;;     :filter-event
;;     :delegate-event))
;; (in-package :clevelib.core)
(defpackage :clevelib.core
  (:use :cl :clevelib.synchronization)
  (:export :event
    :make-event
    :register-handler
    :unregister-handler
    :trigger-event
    :find-handlers
    :*handlers*
    :make-event-listener
    :event-handler
    :filter-event
    :delegate-event
    :event-data
    :event-listener
    :event-listener-event
    :event-listener-target
    :event-listener-callback
    :event-listener-filter
    :dispatch-event
    :event-listener-delegate
    :clear-handlers
    :add-event-listener))
(in-package :clevelib.core)

(defclass event ()
  ((type :initarg :type :accessor event-type)
    (target :initarg :target :accessor event-target)
    (timestamp :initarg :timestamp :accessor event-timestamp)
    (data :initarg :data :accessor event-data)))

(defvar *handlers* (make-hash-table :test 'equal))
(defgeneric filter-event (event target callback))
;; filter events are called before the event is dispatched to the target.
;; if the filter returns true, the event is not dispatched to the target.
;; if the filter returns false, the event is dispatched to the target.
;; (defmethod filter-event )

(defgeneric delegate-event (event target callback))
(defmethod delegate-event (event target callback) t)

(defun make-event-listener (&key event event-type target callback filter delegate)
  (let ((listener (make-instance 'event-listener :event event :event-type event-type :target target :callback callback :filter filter :delegate delegate)))
    (let ((existing-listeners (gethash event *handlers*)))
      (setf (gethash event *handlers*) (cons listener existing-listeners)))))

(defun add-event-listener (event-type  target callback &key  filter delegate)
  (make-event-listener :event event-type :event-type event-type :callback callback :target target :filter filter :delegate delegate))
;; (defmethod delegate-event ((event event) target callback)
;;   "Delegates an event to another object. If the event has a delegate, the delegate
;; is called with the event, target, and callback. If the delegate returns true, the
;; event is not dispatched to the target. If the delegate returns false, the event
;; is dispatched to the target."
;;   (let ((delegate (event-delegate event)))
;;     (if delegate
;;       (funcall delegate event target callback)
;;       t)))

(defun clear-handlers ()
  "Clears all handlers."
  (clrhash *handlers*)
  )
(defclass event-listener ()
  ((event :initarg :event :accessor event-listener-event)
    (event-type :initarg :event-type :accessor event-listener-event-type)
    (target :initarg :target :accessor event-listener-target)
    (callback :initarg :callback :accessor event-listener-callback)
    (filter :initarg :filter :accessor event-listener-filter)
    (delegate :initarg :delegate :accessor event-listener-delegate)
    (options :initform (make-hash-table) :type hash-table :initarg :options :accessor event-listener-options)))

;; (defclass event-handler ()
;;   ((event-type :initarg :event-type :accessor event-handler-event-type)
;;     (callback :initarg :callback :accessor event-handler-callback)
;;     (target :initarg :target :accessor event-handler-target)
;;     (options :initform (make-hash-table) :type hash-table :initarg :options :accessor event-handler-options)))

(defun make-event (type target &key data)
  (make-instance 'event :type type :target target :timestamp (get-universal-time) :data data))

;; (defun make-event-handler (event-type callback &key target options)
;;   (make-instance 'event-handler
;;     :event-type event-type
;;     :callback callback
;;     :target target
;;     :options (alexandria:plist-hash-table options)))


;; (defun register-handler (event-type callback &key target options)
;;   (let ((handler (make-event-handler event-type callback :target target :options options)))
;;     (setf (gethash event-type *handlers*)
;;       (cons handler (gethash event-type *handlers*)))))

;; (defun unregister-handler (event-type callback &key target options)
;;   (let ((handler (make-event-handler event-type callback :target target :options options)))
;;     (setf (gethash event-type *handlers*)
;;       (remove handler (gethash event-type *handlers*) :test #'equal))))

(defun trigger-event (event-type &rest args)
  (let ((event (apply #'make-event event-type args)))
    (dispatch-event event)))


(defun find-handlers (event-type &key target)
  (let ((handlers (gethash event-type *handlers*)))
    ;; (format t "Finding handlers for ~A~%" event-type)
    ;; (format t "Handlers: ~A~%" handlers)
    (remove-duplicates
      (remove-if-not (lambda (handler)
                       ;; (format t "Checking handler ~A~%" handler)
                       ;; (format t "Event type: ~A~%" (event-listener-event-type handler))
                       ;; (format t "Target: ~A~%" (event-listener-target handler))
                       (and (eq (event-listener-event-type handler) event-type)
                         (or (null target)
                           (eq (event-listener-target handler) target))))
        handlers)
      :from-end t
      :test 'equal)))



(defun dispatch-event (event)
  ;; (format t "Dispatching event ~A " event)
  (let* ((event-type (event-type event))
          (target (event-target event))
          (handlers (find-handlers event-type :target target)))
    ;; (format t "Dispatching event ~A to ~A handlers~%" event (length handlers))
    ;; (format t "Handlers: ~A~%" *handlers*)
    (dolist (handler handlers)
      (funcall (event-listener-callback handler) event))))
