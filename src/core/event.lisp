(defpackage :clevelib.core
  (:use :cl )
  (:export :event
    :make-event
    :register-handler
    :event-type
    :unregister-handler
    :trigger-event
    :find-handlers
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
    :add-event-listener
    :event-listener-delegate
    :clear-handlers
    :*listeners*

    :make-event-listener)
  (:import-from :cl.state :*state*))

(in-package :clevelib.core)

(defvar *listeners* (cl.state::state-listeners *state*) "A hash table of event handlers.")

(defclass event ()
  ((type :initarg :type :accessor event-type)
    (target :initarg :target :accessor event-target)
    (timestamp :initarg :timestamp :accessor event-timestamp)
    (data :initarg :data :accessor event-data)))

(defgeneric filter-event (event target callback))
;; filter events are called before the event is dispatched to the target.
;; if the filter returns true, the event is not dispatched to the target.
;; if the filter returns false, the event is dispatched to the target.
;; (defmethod filter-event )

(defgeneric delegate-event (event target callback))
(defmethod delegate-event (event target callback) t)

(defun make-event-listener (&key event event-type target callback filter delegate )
  "Creates an event listener. The event listener is added to the global event
listener hash table. The event listener is returned.
  Arguments:
    event - The event to listen for. If the event is not nil, the event-type is ignored.
    event-type - The event type to listen for.
    target - The target object to call when the event is dispatched.
    callback - The callback function to call when the event is dispatched. The callback function is called with the event.
    filter - Filters an event before it is dispatched to the target. If the filter returns true, the event is not dispatched to the target. If the filter returns false, the event is dispatched to the target.
    delegate - Delegates an event to another object. If the event has a delegate, the delegate is called with the event, target, and callback. If the delegate returns true, the event is not dispatched to the target. If the delegate returns false, the event is dispatched to the target.
  Returns:
    The event listener.
  Example:
    (make-event-listener :event-type :my-event :target my-object :callback #'my-callback)
    (make-event-listener :event (make-event :my-event) :target my-object :callback #'my-callback)
    (make-event-listener :event-type :my-event :target my-object :callback #'my-callback :filter #'my-filter)"
  (let ((listener (make-instance 'event-listener :event event :event-type event-type :target target :callback callback :filter filter :delegate delegate)))
    (let ((existing-listeners (gethash event *listeners*)))
      (setf (gethash event *listeners*) (cons listener existing-listeners)))))

(defun add-event-listener (event-type  target callback &key  filter delegate)
  "Adds an event listener to the global event listener hash table. The event listener is returned.
  Arguments:
    event-type - The event type to listen for.
    target - The target object to call when the event is dispatched.
    callback - The callback function to call when the event is dispatched. The callback function is called with the event.
    filter - Filters an event before it is dispatched to the target.
If the filter returns true, the event is not dispatched to the target. If the filter returns false, the event is dispatched to the target.
    delegate - Delegates an event to another object. If the event has a delegate, the delegate is called with the event, target, and callback. If the delegate returns true, the event is not dispatched to the target.
If the delegate returns false, the event is dispatched to the target.
  Returns:
    "

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
  (clrhash *listeners*)
  )
(defclass event-listener ()
  ((event :initarg :event :accessor event-listener-event
     :documentation "The event to listen for. If the event is not nil, the event-type is ignored." )
    (event-type :initarg :event-type :accessor event-listener-event-type
      :documentation "The event type to listen for.")
    (target :initarg :target :accessor event-listener-target :documentation "The target object to call
when the event is dispatched.")
    (callback :initarg :callback :accessor event-listener-callback :documentation "The callback function to call when the event is dispatched.
The callback function is called with the event.")
    (filter :initarg :filter :accessor event-listener-filter
      :documentation "Filters an event before it is dispatched to the target. If the filter returns
true, the event is not dispatched to the target. If the filter returns false,
the event is dispatched to the target.")
    (delegate :initarg :delegate :accessor event-listener-delegate
      :documentation "Delegates an event to another object. If the event has a delegate, the delegate
is called with the event, target, and callback. If the delegate returns true,
the event is not dispatched to the target. If the delegate returns false, the
event is dispatched to the target.")
    (options :initform (make-hash-table) :type hash-table :initarg :options :accessor event-listener-options
      :documentation "A hash table of options for the event listener.")))

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
;;     (setf (gethash event-type *listeners*)
;;       (cons handler (gethash event-type *listeners*)))))

;; (defun unregister-handler (event-type callback &key target options)
;;   (let ((handler (make-event-handler event-type callback :target target :options options)))
;;     (setf (gethash event-type *listeners*)
;;       (remove handler (gethash event-type *listeners*) :test #'equal))))

(defun trigger-event (event-type &rest args)
  (let ((event (apply #'make-event event-type args)))
    (dispatch-event event)))


(defun find-handlers (event-type &key target)
  (let ((handlers (gethash event-type *listeners*)))
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
    ;; (format t "Handlers: ~A~%" *listeners*)
    (dolist (handler handlers)
      (funcall (event-listener-callback handler) event))))
