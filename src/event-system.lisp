(defpackage :clevelib.event-system
  ;; (:nicknames :cl-es)
  ;; (:documentation "An Event System to facilitate communication between components.")
  (:use :cl :bt :log4cl )
  ;; aliases : est
  (:export
    :event-system
    :idempotent-add-loops
    :event-system-pool
    ))
(in-package :clevelib.event-system)

(defclass event-system ()
  ((pool :initform (clevelib.threads:make-thread-pool) :accessor event-system-pool
     :documentation "A thread pool to handle asyncronicity.")
    (loops :initform nil :accessor event-system-loops :documentation "A list of loops that allow
asynchronous event management."))
  (:documentation "An Event System to facilitate communication between components."))


(defmethod initialize-instance :after ((event-system event-system) &key)
  "Initialize the event system."
  )

(defmethod idempotent-add-loops ((event-system event-system) &rest loops)
  "Add LOOPS to the event system. If the loop is already present, it is not added.
  This method is idempotent."
  (dolist (loop loops)
    (unless (member loop (event-system-loops event-system) :test #'equal)
      (push loop (event-system-loops event-system)))))

;;; Event System
;;; ============
;;; The event system is a way to communicate between components asynchronously.
;;; It is based on the idea of a loop that is running in a thread and that is
;;; waiting for events to happen. When an event happens, the loop calls the
;;; associated handlers.
