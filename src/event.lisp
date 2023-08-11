(in-package :clevelib.event-system)

(defclass event ()
  ((type :initarg :type
     :accessor event-type
     :documentation "The type of the event.")
    (time :initarg :timestamp
      :accessor event-time
      :documentation "The time of the event.")
    (data :initarg :data
      :accessor event-data
      :documentation "The data of the event.")
    (phase :initarg :phase
      :accessor event-phase
      :documentation "The phase of the event.
Possible values are :init, :capturing, :target and :bubbling,
:done, :stopped.")
    (target :initarg :target
      :accessor event-target
      :documentation "The original target of the event.")
    (current-target :initarg :current-target
      :accessor event-current-target
      :documentation "The current target of the event propagation."))
  (:documentation "An event is a message that is sent to an object. The object
can then react to the event by calling the appropriate handlers."))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "<Event[~a|~a|~a]>" (event-type event)
      (event-target event) (event-data event))))

(defun make-event (event-type target &optional data)
  (make-instance 'event
    :type event-type :target target :timestamp (get-universal-time)
    :current-target target :phase :init :data data))

(defmethod set-phase ((event event) phase)
  "Set the phase of the event."
  (declare (type event event))
  (unless (member phase '(:init :capturing :target :bubbling :done :stopped))
    (error "Invalid phase: ~a" phase))
  (setf (event-phase event) phase))

(defmethod propagation-stopped ((event event))
  "Stop the propagation of the given event."
  (eq (event-phase event) :stopped))

(defmethod stop-propagation ((event event))
  "Stop the propagation of the given event."
  (set-phase event :stopped))
