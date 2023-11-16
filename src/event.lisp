(in-package :clevelib.event-system)

(defclass event ()
  ((type :initarg :type
     :initform nil
     :accessor event-type
     :documentation "The type of the event.")
    (time :initarg :timestamp
      :initform nil
      :accessor event-time
      :documentation "The time of the event.")
    (data :initarg :data
      :initform nil
      :accessor event-data
      :documentation "The data of the event.")
    (priority :initarg :priority
      :initform 0
      :accessor event-priority
      :documentation "The priority of the event.")
    (phase :initarg :phase
      :initform nil
      :accessor event-phase
      :documentation "The phase of the event.
Possible values are :init, :capturing, :target and :bubbling,
:broadcast, :done, :stopped.")
    (target :initarg :target
      :initform nil
      :accessor event-target
      :documentation "The original target of the event.")
    (current-target :initarg :current-target
      :initform nil
      :accessor event-current-target
      :documentation "The current target of the event propagation."))
  (:documentation "An event is a message that is sent to an object. The object
can then react to the event by calling the appropriate handlers."))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "<Event[~a|~a|~a]>" (event-type event)
      (event-target event) (event-data event))))

;; (defmethod initialize-instance :after ((e event) &key)
;;   )
;; (defun make-event (event-type target &optional data (priority 0))
;;   (make-instance 'event
;;     :type event-type :target target :timestamp (get-universal-time)
;;     :priority priority :current-target target :phase :init :data data))

(defmethod set-phase ((event event) phase)
  "Set the phase of the event."
  (declare (type event event))
  (unless (member phase '(:init :capturing :target :bubbling :done :broadcast :stopped))
    (error "Invalid phase: ~a" phase))
  (setf (event-phase event) phase))

(defmethod propagation-stopped ((event event))
  "Stop the propagation of the given event."
  (eq (event-phase event) :stopped))

(defmethod stop-propagation ((event event))
  "Stop the propagation of the given event."
  (set-phase event :stopped))
;; Define a base class for events
;; (defclass event ()
;;   ((data :accessor event-data
;;          :initarg :data
;;          :initform nil))
;;   (:documentation "Base class for all events"))

;; A macro to generate subclasses of the event class
(defmacro defevent (name &rest slots)
  "Macro for defining event subclasses."
  `(progn
     (defclass ,name (event)
       ,@slots
       (:documentation ,(format nil "A ~A event class" name)))
     (defmethod print-object ((event ,name) stream)
       (print-unreadable-object (event stream :type t :identity t)
         (format stream "<Event~a[~a|~a]>" ,(symbol-name name)
           (event-target event) (event-data event))))))
;; (defgeneric make-event (event-type &rest args)
;;   (:documentation "Create an event instance, using either the base class or a subclass if it exists."))
;; (defmethod make-event (event-type &rest args))
;;   "Create an event instance, using either the base class or a subclass if it exists."
;;   (if (find-class event-type nil)
;;       (apply #'make-instance event-type args)
;;       (apply #'make-instance 'event args))
;; (defmethod make-event (()))
;; (defevent my-event ((extra-data :accessor my-event-extra-data :initarg :extra-data)))

;; Function to make event instances dynamically
(defun make-event (event-type &rest args)
  "Create an event instance, using either the base class or a subclass if it exists.
The event-type argument is a symbol naming the event class to create. The remaining
arguments are passed to the event class constructor.
If the event-type is not a valid class name, an instance of the base event class is
created.
  Arguments:
    event-type - The type of event to create.
    args - The arguments to pass to the event constructor.
  Returns:
    An instance of the event class or the specified subclass.
  Example:
    (make-event 'my-event :data \"Subclass event\" :extra-data \"Extra data\")
    (make-event 'event :data \"Base event\")"
  (log-debug "Creating event of type ~a with args ~a. find class: ~a" event-type args
    (find-class event-type nil))
  (if (find-class event-type nil)
    (apply #'make-instance event-type (append args (list :type event-type)))
    (apply #'make-instance 'event (append (list :type event-type) args))))
;; (make-event 'event :data "Base event")
;; (make-event 'key-event :data "Base event")
;; ;; Example usage
;; (defevent my-event ((extra-data :accessor my-event-extra-data :initarg :extra-data)))

;; ;; Create an instance of the base event class
;; (let ((e1 (make-event 'event :data "Base event")))
;;   (format t "Event Data: ~A~%" (event-data e1)))

;; ;; Create an instance of the my-event subclass
;; (let ((e2 (make-event 'my-event :data "Subclass event" :extra-data "Extra data")))
;;   (format t "Event Data: ~A, Extra Data: ~A~%" (event-data e2) (my-event-extra-data e2)))

;; (make-event 'dud :data "dud")
(defevent key-event
  ((key-sequence :accessor key-event-key-sequence :initarg :data)))
;; (defvar z (make-instance 'key-event :key-sequence "C-x C-c"))
;; (defvar aa (make-event 'key-event :key-sequence "C-x C-c" :target nil :data nil))
;; (defvar zzzz (make-event 'wurst-event :target nil :data nil))
;; (defvar ab (make-event 'wurst-event :target nil :data nil))
