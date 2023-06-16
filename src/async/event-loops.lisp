(defpackage :clevelib.event-loops
  (:use #:cl #:bt #:clevelib.queues :clevelib.core
    )
  (:export #:enqueue-event
    #:make-event-loop
    #:get-event-loop
    #:event-loop-active-p
    #:enqueue-event
    #:process-single-event
    #:trigger-error-event
    #:process-loop-events
    #:start-event-loop
    #:stop-event-loop
    #:toggle-event-loop
    #:error-event-handler
    ))

(in-package #:clevelib.event-loops)
;; event.lisp

(defvar *error-event-priority* 10)
(defvar *event-queue* (clevelib.queues:make-event-queue ))
(defvar *event-loops-active* (make-hash-table :test 'equal))
(defvar *event-lock* (bt:make-lock "event lock"))
(defvar *event-loops* (make-hash-table :test 'equal))


;; (defmethod trigger-event ((event event) (target t) (data t))
;;   "Trigger an event."
;;   (let ((handler (get-event-handler event target)))
;;     (if handler
;;       (funcall handler event target data)
;;       (trigger-error-event event target))))


(defun get-event-loop (id)
  (gethash id *event-loops*))

(defun event-loop-active-p (id)
  (member id *event-loops-active*))


(defun process-single-event (event-priority-queue)
  "Process a single event in the given event-priority-queue."
  (bt:with-lock-held (*event-lock*)
    (let ((event (dequeue-event event-priority-queue)))
      (when event
        (handler-case
          (clevelib.core:dispatch-event event)
          (error (c)
            (format *error-output* "~&Error processing event: ~A~%" c)))))))

(defclass event-loop ()
  ((id :initarg :id :initform nil :accessor id)
    (queue :initarg :queue
      :initform (make-instance 'priority-event-queue)
      :accessor queue)
    (thread :initarg :thread
      :initform nil
      :accessor thread)
    (lock :initarg :lock
      :initform (bt:make-lock "event lock")
      :accessor lock)
    (condition-var :initarg :condition
      :initform (bt:make-condition-variable)
      :accessor condition-var)
    (active :initarg :active
      :initform nil
      :accessor active)))

(defun make-event-loop ()
  "Make an event loop."
  (make-instance 'event-loop :id (gensym)))

(defmethod notify-event ((ev-loop event-loop))
  "Notify an event loop that an event has occurred."
  (bt:condition-notify (condition-var ev-loop)))

(defmethod enqueue-event ((ev-loop event-loop) event)
  "Enqueue an event in the event queue."
  (bt:with-lock-held ((lock ev-loop))
    (clevelib.queues:enqueue event (queue ev-loop))
    (notify-event ev-loop)))


(defun start-event-loop (ev-loop)
  "Start an event loop."
  (unless (active ev-loop)
    (setf (active ev-loop) t)
    (setf (thread ev-loop) (bt:make-thread
                             (lambda ()
                               (loop while (active ev-loop)
                                 do (process-loop-events ev-loop)))))))

(defun stop-event-loop (ev-loop)
  "Stop the event loop."
  (when (active ev-loop)
    (setf (active ev-loop) nil)))

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

(defvar *event-lock* (bt:make-lock "event lock")) ;; Use reader-writer lock
;; (defun process-loop-events ()
;;   "Process the events in the queue."
;;   (let ((event-loop-hash (gethash id *event-loops*))) ;; Cache lookup
;;     (loop
;;       (let ((event (bt:with-recursive-lock-held (*event-lock*)
;;                      (dequeue *event-queue*))))
;;         (when event
;;           (trigger-event event (event-target event) (event-data event)))))))
