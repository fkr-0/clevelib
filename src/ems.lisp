(uiop:define-package :clevelib.ems
  (:use :cl :log4cl)
  (:import-from :clevelib.event
    :event)
  (:import-from :clevelib.queue
    :enqueue)
  (:import-from :clevelib.event-loop
    :i-queue)
  (:import-from :clevelib.api
    :on-message
    :on-event
    :add-response-handler
    :add-async-exec-handler
    :ems-root
    :get-children
    :get-parent
    :translate-event-to-message
    :relay-message)
  (:import-from :clevelib.message
    :async-handle
    :message-state
    :message-type
    :message-data
    :reenqueue-handle
    :message
    :event-message)
  (:import-from :clevelib.thread-pool :start-thread :make-thread-pool)
  (:documentation "Event Management System")
  (:export :emitter
    :messenger
    :on-message
    :ems
    :ems-pool
    :emit
    :emission-sinks
    :add-response-handler
    :add-async-exec-handler
    :append-hash))

(in-package :clevelib.ems)

(defclass ems ()
  ((root :initform nil :accessor ems-root-acc :initarg :root)
    (message-loop :initform nil
      :accessor ems-message-loop
      :documentation "The message loop for this relay")
    (pool :initform (make-thread-pool)
      :accessor ems-pool
      :documentation "A thread pool to handle asyncronicity."))
  (:documentation "Event Management System"))

(defmethod print-object ((ems ems) stream)
  "Print the EMS to STREAM."
  (print-unreadable-object (ems stream :type t :identity t)
    (format stream "~a|Root:~a|Pool:~a"
      (type-of ems)
      (ems-root-acc ems)
      (ems-pool ems))))

(defmethod ems-root ((ems ems) &rest args)
  (declare (ignorable args))
  (ems-root-acc ems))
(defmethod (setf ems-root) (new-value (ems ems) &rest args)
  (declare (ignorable args))
  (setf (ems-root-acc ems) new-value))

(defmethod initialize-instance :after ((relay ems) &key)
  (setf (ems-message-loop relay)
    (make-instance 'clevelib.event-loop:event-loop
      :wait t
      :fps 10
      :loop-fun (lambda (m) (relay-message relay m)))))

(defmethod translate-event-to-message ((ems ems) (e event) &rest args)
  ""
  (declare (ignorable args))
  (make-instance 'event-message :event e))

(defmethod on-event ((ems ems) event &rest args)
  "handle the event by translating it to a message
and sending it to the message bus."
  (declare (ignorable args))
  (on-message ems (translate-event-to-message ems event)))

(defmethod enqueue-message ((ems ems) message)
  (clevelib.queue:enqueue
    (clevelib.event-loop:i-queue
      (ems-message-loop ems))
    message))



(defmethod add-async-exec-handler ((ems ems) (m message) &rest args)
  "Return a function that can be used to execute a function in the relay's thread pool.
  The function will enqueue the result of the function in the relay's message queue."
  (declare (ignorable args))
  (setf (async-handle m)
    ;; (apply #'clevelib.thread:submit-job (event-system-pool relay) function args)
    (lambda (function &rest args)
      (apply #'clevelib.thread-pool:start-thread
        (list (ems-pool ems)
          (lambda ()
            (let ((r (apply function args)))
              (when r (enqueue-message ems r))))
          :description (format nil "Relay ~a" "placeholder")))))) ; callback to enque async tasks

(defmethod add-response-handler ((ems ems)
                                  (m message) &rest args)
  (declare (ignorable args))
  (setf (reenqueue-handle m)
    (lambda (new-message)
      (on-message ems new-message))))

(defmethod on-message ((ems ems) message &rest args)
  (declare (ignorable args))
  (enqueue-message ems message))


(defmethod start ((ems ems))
  (clevelib.thread-pool:start-thread (ems-pool ems)
    (lambda ()
      (log-debug "Ems ~a started" ems)
      (clevelib.event-loop:run-loop (ems-message-loop ems)))
    :description (format nil "Relay ~a Event Loop Thread" ems)))

(defmethod running-p ((ems ems))
  (clevelib.event-loop:event-loop-active-p (ems-message-loop ems)))

(defmethod stop ((ems ems))
  (clevelib.event-loop:stop-loop (ems-message-loop ems)))



(defclass messenger ()
  ((name :initarg :name :accessor messenger-name :documentation "The name of the entity." :initform (symbol-name (gensym)))
    (parent :initform nil :accessor messenger-parent :documentation "The parent entity." :initarg :parent)
    (handlers :initform (make-hash-table :test #'equal) :accessor entity-handlers :documentation "A hash table of message handlers.")
    (children :initform nil :accessor messenger-children
      :documentation "The list of child entities."))
  (:documentation "An entity that supports propagation in a hierarchical structure."))

(defmethod print-object ((entity messenger) stream)
  "Print the PROPAGATION-ENTITY to STREAM."
  (print-unreadable-object (entity stream :type t :identity t)
    (format stream "~a|Parent:~a|Children:~a"
      (messenger-name entity)
      (and (messenger-parent entity) (messenger-name (messenger-parent entity)))
      (length (messenger-children entity)))))

(defmethod initialize-instance :after ((entity messenger) &key parent)
  (setf (messenger-children entity) nil)
  (if parent
    (add-child parent entity)))

(defmethod get-children ((entity messenger))
  (messenger-children entity))

(defmethod get-parent ((entity messenger))
  (messenger-parent entity))

(defmethod add-child ((entity messenger) child)
  (push child (messenger-children entity))
  (setf (messenger-parent child) entity))

