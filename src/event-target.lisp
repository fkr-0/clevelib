(in-package :clevelib.event-system)

(defclass event-target ()
  ((emitter :initarg :emitter
     :accessor event-target-emitter
     :documentation "The event emitter of the object.")
    (register :initarg :register
      :accessor event-target-register
      :documentation "Callable that allows registering
handler functions for events.")
    (unregister :initarg :unregister
      :accessor event-target-unregister
      :documentation "Callable that allows unregistering
handler functions for events.")
    (handlers :initform (make-hash-table)
      :accessor handlers
      :documentation "A hash table of handlers for the widget.")
    (children :initarg :children
      :accessor event-target-children
      :documentation "The children of the object."))
  (:documentation "An event target is an object that is assigned closures to
emit events, add and remove event handlers."))

(defmethod initialize-instance :after ((target event-target) &key (app nil))
  (when app (set-up-target app target)))

(defun make-event-target (&key emitter register unregister children)
  (make-instance 'event-target :emitter emitter
    :register register :unregister unregister :children children))

(defmethod register ((target event-target) event-type handler &key (phase :at-target))
  "Register a handler for the given event type.
    The handler is called with the event as the first argument and the optional data as the second argument."
  (declare (type event-target target))

  (if (and (slot-boundp target 'register)
        (event-target-register target))

    (let ((r (event-target-register target)))
      (funcall r event-type handler ))
    (setf (gethash event-type (handlers target))
      (make-instance 'event-handler
        :event-type event-type
        :callback handler
        :options phase))))

(defmethod unregister ((target event-target) event-type handler &key phase)
  "Unregister a handler for the given event type."
  (declare (type event-target target))
  (let ((unregister (event-target-unregister target)))
    (unless (event-target-unregister target)
      (error "No unregister method for target: ~a" target))
    (funcall unregister event-type handler phase)))



(defmethod register ((target event-target) event-type handler &key (phase :at-target))
  "Register a handler for the given event type.
    The handler is called with the event as the first argument and the optional data as the second argument."
  (declare (type event-target target))

  (if (and (slot-boundp target 'register)
        (event-target-register target))

    (let ((r (event-target-register target)))
      (funcall r event-type handler ))
    (setf (gethash event-type (handlers target))
      (make-instance 'event-handler
        :event-type event-type
        :callback handler
        :options phase))))

(defmethod target-children ((target event-target))
  "Get the children of the target."
  (let ((children (event-target-children target)))
    (unless children
      (error "No children for target: ~a" target))
    (funcall children)))

(defmethod target-emit-event ((target event-target) event-type &optional ev-target data)
  "Emit an event of the given type on the target, passing the optional data to the event handler."
  (declare (type event-target target))
  (let ((emitter (event-target-emitter target)))
    (unless emitter
      (error "No emitter for target: ~a" target))
    (funcall emitter  event-type ev-target  data)))

(defgeneric set-up-target (system target)
  (:documentation "Set up a target for the event system."))
