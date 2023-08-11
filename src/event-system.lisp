(defpackage :clevelib.event-system
  ;; (:nicknames :cl-es)
  ;; (:documentation "An Event System to facilitate communication between components.")
  (:use :cl :bt :log4cl :clevelib.hierarchy)
  ;; aliases : est
  (:export
    :get-children
    :next-leaf-with-prop
    :target-children
    :hierarchy
    :register
    :unregister
    :emit-event
    :handlers
    :event-system
    :get-leafs
    :add-handler
    :add-target
    :event-target-emitter
    :target-emit-event
    :event-target-register
    :event-target-unregister
    :event-target-children
    :get-root-target
    :add-root-target
    :remove-root-target
    :remove-handler
    :event-type
    :event-phase
    :dispatch
    :get-target-path
    :event-data
    :event
    :event-time
    :make-event
    :idempotent-add-loops
    :event-system-pool
    :event-system-loops
    :event-target
    :join-pool
    :stat-loops))

(in-package :clevelib.event-system)



;;; Event System
;;; ============
;;; The event system is a way to communicate between components asynchronously.
;;; It is based on the idea of a loop that is running in a thread and that is
;;; waiting for events to happen. When an event happens, the loop calls the
;;; associated handlers.
(defclass event-system ()
  ((hierarchy :initarg :hierarchy
     :initform (clevelib.hierarchy:make-hierarchy)
     :accessor hierarchy
     :documentation "The hierarchy of objects in the system.")
    (pool :initform (clevelib.threads:make-thread-pool)
      :accessor event-system-pool
      :documentation "A thread pool to handle asyncronicity.")
    (handlers :initform (make-hash-table)
      :accessor handlers
      :documentation "The event handlers for the system.")
    (loops :initform nil :accessor event-system-loops
      :documentation "A list of loops that allow
asynchronous event management."))
  (:documentation "An Event System to facilitate communication between components."))

(defmethod initialize-instance :after ((event-system event-system) &key)
  "Initialize the event system.")

(defmethod add-root-target ((event-system event-system) target)
  "Add a target to the event system."
  (declare (type event-system event-system))
  (setf (clevelib.hierarchy:root (hierarchy event-system)) target)
  (clevelib.hierarchy:add-object (hierarchy event-system) target nil))

(defmethod add-target ((event-system event-system) target parent)
  "Add a target to the event system."
  (declare (type event-system event-system))
  (clevelib.hierarchy:add-object (hierarchy event-system) target parent)
  (log-debug "Added target ~a with parent ~a to event system ~a." target parent event-system))
;; (log-debug "Path: ~a" (path (hierarchy event-system) target))

(defmethod get-root-target ((event-system event-system))
  "Get the root target of the event system."
  (declare (type event-system event-system))
  (clevelib.hierarchy:root (hierarchy event-system)))

(defmethod get-target-path ((event-system event-system) target)
  "Get the path from the root to the given target."
  (declare (type event-system event-system))
  ;; (log-debug "Path: ~a" (path (hierarchy event-system) target))
  (clevelib.hierarchy:path (hierarchy event-system) target))

(defmethod get-path ((system event-system) object)
  "Get the path from the root to the given object in the given event system."
  (if object
    (cons object (get-path system (gethash object (hierarchy system))))
    '()))

(defmethod get-parent ((event-system event-system) target)
  "Get the parent of the given target."
  (declare (type event-system event-system))
  (gethash target (clevelib.hierarchy:objects (hierarchy event-system)) ))

(defmethod get-children ((event-system event-system) target)
  "Get the children of the given target."
  (declare (type event-system event-system))
  (let ((children nil))
    (maphash (lambda (key value)
               (when (eq value target)
                 (push key children)))
      (clevelib.hierarchy:objects (hierarchy event-system)))
    children))

(defmethod get-leafs ((event-system event-system))
  "Get the leafs of the event system."
  (declare (type event-system event-system))
  (let ((leafs nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (null (get-children event-system key))
                 (push key leafs)))
      (clevelib.hierarchy:objects (hierarchy event-system)))
    leafs))

(defmethod next-leaf-with-prop ((system event-system) object prop &key (full-path nil) (rev nil))
  "Get the next leaf in the hierarchy of the given event system that has the given
property."
  (let* ((ls (if rev (get-leafs system) (reverse (get-leafs system))))
          (leafs (cdr (append (member object ls) ls)))
          (next-obj (find-if (lambda (x) (funcall prop x)) leafs)))
    (when (and next-obj (not (eq next-obj object)))
      (if full-path
        (get-path system next-obj)
        next-obj))))
(defmethod print-hierarchy ((event-system event-system))
  "Print the hierarchy of the event system."
  (declare (type event-system event-system))
  (let ((paths nil))
    (dolist (leaf (get-leafs event-system))
      (push (get-target-path event-system leaf) paths))
    (format t "~a~%" paths)))


(defmethod join-pool ((event-system event-system))
  "Join all the threads in the pool."
  (clevelib.threads:join-pool-threads (event-system-pool event-system)))

(defmethod idempotent-add-loops ((event-system event-system) &rest loops)
  "Add LOOPS to the event system. If the loop is already present, it is not added.
    This method is idempotent."
  (dolist (loop loops)
    (unless (member loop (event-system-loops event-system) :test #'equal)
      (push loop (event-system-loops event-system)))))

(defmethod add-handler ((system event-system) object event-type callback &key (options :at-target))
  "Add an event handler for the given event type to the given object."
  (let ((handler
          (make-instance 'event-handler
            :event-type event-type
            :callback callback
            :options options))
         (object-handlers (or (gethash object (handlers system))
                            (setf (gethash object (handlers system))
                              (make-hash-table)))))
    (setf (gethash event-type object-handlers) handler)))

(defmethod remove-handler ((system event-system) object event-type)
  "Remove the event handler for the given event type from the given object."
  (let ((object-handlers (gethash object (handlers system))))
    (when object-handlers
      (remhash event-type object-handlers))))

(defmethod remove-event-handler ((event-system event-system) target type callback &key (options nil))
  "Remove an event handler from the event system."
  (declare (type event-system event-system))
  (let ((handlers (gethash target (handlers event-system))))
    (setf (gethash target (handlers event-system))
      (remove-if (lambda (handler)
                   (and (eq (event-handler-event-type handler) type)
                     (eq (event-handler-callback handler) callback)
                     (eq (event-handler-options handler) options)))
        handlers))))
;; (log-debug "Removed event handler ~a from target ~a." handler target)
(defmethod call-handler ((system event-system)
                          object event-type event data phase)
  "Call the handler for the given event type on the given object, if it exists and matches the given phase."
  (let ((handler
          (gethash event-type (or (gethash object (handlers system))
                                (make-hash-table)))))
    (when handler
      (let ((options (event-handler-options handler)))
        (when (or (null options)
                (if (listp options)(member phase options)
                  (eq options phase)))
          (let ((callback (event-handler-callback handler)))
            (when callback
              (funcall callback event data))
            (log-debug "Calling handler ~a for event ~a on object ~a with data ~a in phase ~a"
              handler event-type object data phase)))))))

(defmethod dispatch ((system event-system) event)
  "Trigger the given event type on the given target, passing the optional data to the event handler.
    The event is propagated through the hierarchy of objects according to the capturing and bubbling phases."
  ;; Create the event.
  (let* ((target (event-target event))
          (data (event-data event))
          (event-type (event-type event))
          (path (get-target-path system target)))
    ;; Capturing phase: propagate the event from the root to the target, calling capturing handlers.
    (set-phase event :capturing)
    (dolist (object (reverse path))
      (setf (event-current-target event) object)
      (unless (eq object target)
        (call-handler system object event-type event data :capturing)
        (when (propagation-stopped event)
          (setf (event-phase event) :stopped)
          (return))))
    ;; At target: call handlers at the target.
    (set-phase event :target)
    (call-handler system target event-type event data :at-target)
    (unless (propagation-stopped event)
      ;; Bubbling phase: propagate the event from the target to the root, calling bubbling handlers.
      (set-phase event :bubbling)
      (dolist (object (rest path))
        (setf (event-current-target event) object)
        (call-handler system object event-type event data :bubbling)
        (when (propagation-stopped event)
          (return))))))

;; (defmethod trigger-event ((event-system event-system) target event)
;;   "Trigger an event on the given target."
;;   (declare (type event-system event-system))
;;   (let ((handlers (gethash target (handlers event-system))))
;;     (dolist (handler handlers)
;;       (when (and (eq (event-handler-type handler) (event-type event))
;;               (or (null (event-handler-options handler))
;;                 (member :once (event-handler-options handler))
;;                 (member :once-capturing (event-handler-options handler))
;;                 (member :once-bubbling (event-handler-options handler))))
;;         (remove-event-handler event-system target (event-type event) (event-handler-callback handler) :options (event-handler-options handler)))
;;       (when (and (eq (event-handler-type handler) (event-type event))
;;               (or (null (event-handler-options handler))
;;                 (member :once-capturing (event-handler-options handler))
;;                 (member :capturing (event-handler-options handler))))
;;         (funcall (event-handler-callback handler) event))
;;       (when (and (eq (event-handler-type handler) (event-type event))
;;               (or (null (event-handler-options handler))
;;                 (member :once-bubbling (event-handler-options handler))
;;                 (member :bubbling (event-handler-options handler))))
;;         (funcall (event-handler-callback handler) event))
;;       (when (and (eq (event-handler-type handler) (event-type event))
;;               (or (null (event-handler-options handler))
;;                 (member :once (event-handler-options handler))))
;;         (funcall (event-handler-callback handler) event)))))

;; (defmethod trigger-event-capturing ((event-system event-system) target event)
;;   "Trigger an event on the given target and its parents."
;;   (declare (type event-system event-system))
;;   (let ((parent (get-parent event-system target)))
;;     (when parent
;;       (trigger-event-capturing event-system parent event)))
;;   (trigger-event event-system target event))

;; (defmethod trigger-event-bubbling ((event-system event-system) target event)
;;   "Trigger an event on the given target and its children."
;;   (declare (type event-system event-system))
;;   (let ((children (get-children event-system target)))
;;     (dolist (child children)
;;       (trigger-event-bubbling event-system child event)))
;;   (trigger-event event-system target event))

;; (defmethod trigger-event-path ((event-system event-system) target event)
;;   "Trigger an event on the given target and its parents and children."
;;   (declare (type event-system event-system))
;;   (trigger-event-capturing event-system target event)
;;   (trigger-event-bubbling event-system target event))

;; (log-debug "Path: ~a" )))
;; Use the following to debug the event system:
;; (add-event-handler *event-system* *event-system* :event (lambda (event) (log-debug "Event: ~a" event)))
;; (add-event-handler *event-system* *event-system* :event (lambda (event) (log-debug "Event: ~a" event) (print-hierarchy *event-system*)))
;; (add-event-handler *event-system* *event-system* :event (lambda (event) (log-debug "Event: ~a" event) (print-hierarchy *event-system*)))
;;
;; (add-event-handler *event-system* *event-system* :event (lambda (event) (log-debug "Event: ~a" event) (print-hierarchy *event-system*)))
;; (add-and-create-handler x 'c :dud (lambda (event &rest args) (log-debug "Event: ~a" event) (print-hierarchy *event-system*)))
;; (add-and-create-handler x 'c :dud (lambda (event &rest args) (log-debug "Event: ~a ~a" event args) (print-hierarchy *event-system*)))
;; test
;; (add-event-handler *event-system* *event-system* :event (lambda (event &rest args) (log-debug "Event: ~a ~a" event args) (print-hierarchy *event-system*)))
;; (trigger *event-system*  'c (make-instance 'event :type :dud))



;;
;; (defun add-handler (system object handler)
;;   "Add an event handler to the given object."
;;   (let ((object-handlers (or (gethash object (handlers system))
;;                            (setf (gethash object (handlers system)) (make-hash-table)))))
;;     (setf (gethash (event-handler-event-type handler) object-handlers) handler)))

;; (defun remove-handler (system object event-type)
;;   "Remove the event handler for the given event type from the given object."
;;   (let ((object-handlers (gethash object (handlers system))))
;;     (when object-handlers
;;       (remhash event-type object-handlers))))


;; (defmethod trigger ((system event-system) target event-type &optional data)
;;   "Trigger the given event type on the given target, passing the optional data to the event handler.
;;     The event is propagated through the hierarchy of objects according to the capturing and bubbling phases."
;;   ;; Create the event.
;;   (let ((event (make-instance 'event :type event-type :target target :current-target target)))
;;     ;; Capturing phase: propagate the event from the root to the target, calling capturing handlers.
;;     (dolist (object (reverse (get-path system target)))
;;       (setf (event-current-target event) object)
;;       (call-handler system object event-type event data :capturing)
;;       (when (propagation-stopped event)
;;         (return)))
;;     ;; At target: call handlers at the target.
;;     (call-handler system target event-type event data :at-target)
;;     (unless (propagation-stopped event)
;;       ;; Bubbling phase: propagate the event from the target to the root, calling bubbling handlers.
;;       (dolist (object (rest (get-path system target)))
;;         (setf (event-current-target event) object)
;;         (call-handler system object event-type event data :bubbling)
;;         (when (propagation-stopped event)
;;           (return))))))


