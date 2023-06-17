
(defpackage :clevelib.event-loops
  (:use #:cl #:bt #:clevelib.queues :clevelib.core :clevelib.threads
    )
  (:export #:enqueue-event
    #:make-event-loop
    #:async
    #:until
    #:with-event-loop
    #:fps
    #:get-event-loop
    #:event-loop-active-p
    #:notify-event
    #:event-loop
    #:enqueue-event
    #:process-single-event
    #:trigger-error-event
    #:process-loop-events
    #:start-event-loop
    #:stop-event-loop
    #:toggle-event-loop
    #:error-event-handler
    )
  (:import-from :cl.state :*state*))

(in-package #:clevelib.event-loops)
;; event.lisp

;; (defvar *event-queue* (clevelib.queues:make-event-queue ))
;; (defvar *event-loops-active* (make-hash-table :test 'equal))
;; (defvar *event-lock* (bt:make-lock "event lock"))
(defvar *event-loops* (cl.state::state-event-loops *state*))


;; (defmethod trigger-event ((event event) (target t) (data t))
;;   "Trigger an event."
;;   (let ((handler (get-event-handler event target)))
;;     (if handler
;;       (funcall handler event target data)
;;       (trigger-error-event event target))))


(defun get-event-loop (id)
  (gethash id *event-loops*))

(defun event-loop-active-p (id)
  (active (get-event-loop id)))


(defun process-single-event (event-priority-queue)
  "Process a single event in the given event-priority-queue."
  (with-mutex (clevelib.queues:priority-queue-mutex event-priority-queue)
    (let ((event (dequeue event-priority-queue)))
      (when event
        (handler-case
          (clevelib.core:dispatch-event event)
          (error (c)
            (format *error-output* "~&Error processing event: ~A~%" c)))))))

(defclass event-loop ()
  ((id :initarg :id :initform nil :accessor id)
    (queue :initarg :queue
      :initform (make-instance 'priority-queue)
      :accessor queue)
    (thread :initarg :thread
      :initform nil
      :accessor thread)
    (lock :initarg :lock
      :initform (clevelib.threads:make-mutex)
      :accessor lock)
    (condition-var :initarg :condition
      :initform (clevelib.threads:make-condition-variable)
      :accessor condition-var)
    (active :initarg :active
      :initform nil
      :accessor active)
    (fps :initform nil
      :accessor fps)
    (until :initarg :until
      :initform t
      :accessor until )
    (async :initarg :async :initform t :accessor async)
    (loop-fun :initarg :loop-fun
      :initform (lambda () (format t "No loop function defined."))
      ;; (loop while t
      ;;   do (process-loop-events ev-loop)))
      :accessor loop-fun
      :documentation "The function that is executed
in the event loop thread.")
    )(:documentation "An event loop."))
(defmacro with-fps (fps &body body)
  "Execute the body with the given fps."
  `(format t "FPS: ~A~%" ,fps)
  (if `(numberp ,fps)
    `(let ((fpsreal (/ 1000000.0 ,fps))
            (time (get-internal-real-time)))
       (progn ,@body)
       (let ((time-diff (- (get-internal-real-time) time)))
         (if (< time-diff fpsreal)
           ;; (progn
           ;;   (format t "Sleep: ~A ~%" (- fpsreal time-diff))
           (sleep (/ (- fpsreal time-diff) 1000000.0))
           (warn "FPS too low! ~A" time-diff))))
    `(progn ,@body)))
(defmethod get-loop-fun ((ev-loop event-loop))
  (lambda ()
    (setf (active ev-loop) t)
    (let ((until (if (functionp (until ev-loop)) (until ev-loop)
                   (lambda () (until ev-loop))))
           (first-time-p t))
      (loop while (or first-time-p
                    (and (active ev-loop) (funcall until)))
        do (progn
             (when first-time-p
               (setf first-time-p nil))
             (with-fps (fps ev-loop)
               (funcall (loop-fun ev-loop))))
        ))
    (setf (active ev-loop) nil) #'(lambda () nil)
    ))

(defun make-event-loop ()
  "Make an event loop."
  (let ((ev-loop (make-instance 'event-loop)))
    (setf (id ev-loop) (gensym "event-loop-"))
    (setf (gethash (id ev-loop) *event-loops*) ev-loop)
    ev-loop))

(defmethod notify-event ((ev-loop event-loop))
  "Notify an event loop that an event has occurred."
  (clevelib.threads:signal-condition (condition-var ev-loop)))

(defmethod enqueue-event ((ev-loop event-loop) event)
  "Enqueue an event in the event queue."
  (with-mutex (lock ev-loop)
    (clevelib.queues:enqueue event (queue ev-loop))
    (notify-event ev-loop)))


(defun start-event-loop (ev-loop)
  "Start an event loop."
  (unless (active ev-loop)
    (setf (active ev-loop) t)
    (if (async ev-loop)
      (setf (thread ev-loop) (bt:make-thread
                               (get-loop-fun ev-loop)))
      (funcall (get-loop-fun ev-loop))))
  ev-loop)

(defmacro curry (function &rest args)
  "Curry a function with the given arguments."
  `(lambda (&rest more-args)
     (apply ,function (append ',args more-args))))

;; keywords: :id :async :until
;; :id -> take loop given by id else create new
;; :async -> start loop in new thread (default t)
;; :until -> t -> loop forever
;;           nil -> loop once
;;           callable -> loop until callable returns nil
(defmacro with-event-loop ((&key (id nil) (fps nil) (async t) (until t)) &body body)
  "Execute the body in the event loop with the given id.
If no id is given, a new event loop is created. If async is
true, the event loop is started in a new thread. If until is
true, the event loop is executed until the body returns. If until
is a callable, the event loop is executed until the callable
returns nil. The event loop is returned."
  (let ((ev-loop (gensym)))
    `(let ((,ev-loop (if ,id
                       (get-event-loop ,id)
                       (make-event-loop))))
       (with-slots (fps loop-fun async until) ,ev-loop
         (setf loop-fun (lambda () (progn ,@body)))
         (setf async ,async)
         (setf fps ,fps)
         (setf until ,until))
       (start-event-loop ,ev-loop)
       ,ev-loop)))


(defun stop-event-loop (ev-loop)
  "Stop the event loop."
  (bt:with-lock-held ((lock ev-loop) )
    (setf (until ev-loop) nil)
    (notify-event ev-loop)))
;; (when (active ev-loop)
;;   (setf (active ev-loop) nil)))

(defun process-loop-events (ev-loop)
  "Process the events in the queue of the loop."
  (loop
    (let ((event (bt:with-lock-held ((lock ev-loop))
                   (dequeue (queue ev-loop)))))
      (when event
        (handler-case
          (clevelib.core:dispatch-event event)
          (error (c)
            (format *error-output* "~&Error processing event: ~A~%" c)))))
    (when (bt:with-lock-held ((lock ev-loop))
            (event-queue-empty-p (queue ev-loop)))
      (bt:with-lock-held ((lock ev-loop))
        ;; Wait on the condition variable until an event is enqueued.
        ;; This will release the lock while waiting.
        (bt:condition-wait (condition-var ev-loop) (lock ev-loop))))))

;; (defun trigger-error-event (event target)
;;   (tri event target :priority *error-event-priority*))









(defun error-event-handler (event target)
  ;; Define custom error handling logic here
  (format t "An error occurred in the event '~A' with target '~A'." event target))

;; (add-event-listener :error t #'error-event-handler)

;; (defvar *event-lock* (bt:make-lock "event lock")) ;; Use reader-writer lock
;; (defun process-loop-events ()
;;   "Process the events in the queue."
;;   (let ((event-loop-hash (gethash id *event-loops*))) ;; Cache lookup
;;     (loop
;;       (let ((event (bt:with-recursive-lock-held (*event-lock*)
;;                      (dequeue *event-queue*))))
;;         (when event
;;           (trigger-event event (event-target event) (event-data event)))))))
