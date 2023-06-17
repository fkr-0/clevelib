(defpackage :clevelib.state
  (:use :cl )
  (:nicknames :cl.state)
  (:export :*state* :state)
  )

(in-package :clevelib.state)
(defclass state ()
  ((values :accessor state-values :initarg :values :initform (make-hash-table))
    (listeners :accessor state-listeners :initarg :listeners :initform (make-hash-table))
    (queues :accessor state-queues :initarg :queues :initform (make-hash-table))
    (threads :accessor state-threads :initarg :threads :initform nil)
    (live-updates :accessor state-live-updates :initarg :live-updates :initform nil)
    (event-classes :accessor state-event-classes :initarg :event-classes :initform (make-hash-table))
    (event-loops :accessor state-event-loops :initarg :event-loops :initform (make-hash-table))
    (targets :accessor state-targets :initarg :targets :initform (make-hash-table))
    (current-state :initform :initializing :accessor state-current-state)
    (previous-state :initform nil :accessor state-previous-state)
    (state-history :initform nil :accessor state-state-history))
  (:documentation "A state machine for the application."))


(defvar *state* nil "The state machine for the application.")
(defvar *error-event-priority* 10)

(defmethod initialize-instance :after ((state state) &key)
  (setf *state* state))

(make-instance 'state)


(defun transition-state (new-state)
  "Transition the state machine to the new state."
  (log-message "Transitioning from state: ~A to state: ~A"
    (state-current-state *state*) new-state)
  (setf (state-previous-state *state*) (state-current-state *state*))
  (log-message "Adding state to history: ~A" (list (state-current-state *state*)
                                               (state-previous-state *state*)))
  (push (cons (state-current-state *state*) (state-previous-state *state*))
    (state-state-history *state*))
  (log-message "Setting current state to: ~A" new-state)
  (setf (state-current-state *state*) new-state))

(defun revert-state ()
  "Revert the state machine to the previous state."
                                        ; Get the previous state from the state machine.
  (let ((previous-state (state-previous-state *state*)))
                                        ; If there is a previous state, set the current state to the previous
                                        ; state and set the previous state to nil.
    (if previous-state
      (progn
        (log-message "Reverting state to ~A" previous-state)
        (setf (state-current-state *state*) previous-state)
        (setf (state-previous-state *state*) nil))
                                        ; If there is no previous state, signal an error.
      (error "No previous state to revert to"))))

(defun get-state-value ( key string )
  "Get the value of the state variable with the given key."
  (gethash key *state*))

(defun set-state-value (key string value)
  "Set the value of the state variable with the given key."
  (setf (gethash key *state*) value))




(defun add-target (target-name target)
  "Add a target to the state machine."
  (setf (gethash target-name (state-targets state))
    target))
(defun get-target (target-name)
  "Get a target from the state machine."
  (gethash target-name (state-targets state)))
(defun add-event-type (event-type construct)
  "Add an event type to the state machine. The construct function is called when the event is dispatched.
The construct function should return an event object or a subclass, event-type
should be a keyword."
  (setf (gethash  event-type (state-event-classes state))
    construct))

(defun add-loop (loop-name loop)
  "Add an event loop to the state machine."
  (setf (gethash loop-name (state-event-loops *state*))
    loop))
(defun get-loop (loop-name)
  "Get an event loop from the state machine."
  (gethash loop-name (state-event-loops *state*)))


(defun dispatch-constructor (event-type &rest args)
  (if (gethash event-type (state-event-classes *state*))
    (apply (gethash event-type (state-event-classes *state*)) args)
    (apply #'make-event  args)))


;; (defun set-state (new-state)
;;   "Set the current state of the application."
;;   (transition-state new-state))

;; (defun add-state-listener (state callback)
;;   "Add a callback to be called when the state transitions to the given state."
;;   (push callback (gethash state (state-listeners *state*))))

;; (defun remove-state-listener (state callback)
;;   "Remove a callback that was added with add-state-listener."
;;   (setf (gethash state (state-listeners *state*))
;;     (remove callback (gethash state (state-listeners *state*)))))

;; (defun call-state-listeners (state)
;;   "Call all listeners for the given state."
;;   (dolist (callback (gethash state (state-listeners *state*)))
;;     (funcall callback)))

(defun transition-state (new-state)
  "Transition the state machine to the new state."
  (call-state-listeners (state-current-state *state*))
  (setf (state-previous-state *state*) (state-current-state *state*))
  (push (cons (state-current-state *state*) (state-previous-state *state*))
    (state-state-history *state*))
  (setf (state-current-state *state*) new-state)
  (call-state-listeners new-state))


(defun revert-state ()
  (let ((previous-state (state-previous-state *state*)))
    (if previous-state
      (progn
        (setf (state-current-state *state*) previous-state)
        (setf (state-previous-state *state*) nil))
      (error "No previous state to revert to"))))

;; (defun get-state ()
;;   "Get the current state of the application."
;;   (state-current-state *state*))

;; (defun set-state (new-state)
;;   "Set the current state of the application."
;;   (transition-state new-state))

;; (defun add-state-listener (state callback)
;;   "Add a callback to be called when the state transitions to the given state."
;;   (push callback (gethash state (state-listeners *state*))))

;; (defun remove-state-listener (state callback)
;;   "Remove a callback that was added with add-state-listener."
;;   (setf (gethash state (state-listeners *state*))
;;     (remove callback (gethash state (state-listeners *state*)))))

;; (defun call-state-listeners (state)
;;   "Call all listeners for the given state."
;;   (dolist (callback (gethash state (state-listeners *state*)))
;;     (funcall callback)))

(defun transition-state (new-state)
  "Transition the state machine to the new state."
  (call-state-listeners (state-current-state *state*))
  (setf (state-previous-state *state*) (state-current-state *state*))
  (push (cons (state-current-state *state*) (state-previous-state *state*))
    (state-state-history *state*))
  (setf (state-current-state *state*) new-state)
  (call-state-listeners new-state))
