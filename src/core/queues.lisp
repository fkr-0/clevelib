;; File: clevelib/queues.lisp
;;
;; Purpose: This file provides an implementation of event queues for managing
;;          events in the event system. It includes functions for enqueuing,
;;          dequeuing, and managing event priorities.
;;
(defpackage :clevelib.queues
  (:use :common-lisp  :clevelib.synchronization)
  (:export :enqueue
    :dequeue
    :change-event-priority
    :event-queue-empty-p
    :event-queue-length
    :event-queue
    :make-event-queue
    :priority-queue
    :enqueue
    :queue
    :size))
(in-package :clevelib.queues)
(defvar *event-queues-mutex* (make-instance 'clevelib.synchronization:event-mutex))

;; (defun enqueue-event (event)
;;   (clevelib:with-event-mutex *event-queues-mutex*
;;     ;; ... enqueue the event to the appropriate queue ...
;;     ))

;; (defun dequeue-event ()
;;   (clevelib:with-event-mutex *event-queues-mutex*
;;     ;; ... dequeue the next event from the appropriate queue ...
;;     ))

(defparameter *event-queue-mutex* (bt:make-lock "event-queue-mutex"))
(defclass priority-queue ()
  ((queue :initform (make-hash-table)
     :accessor queue)
    (mutex :initform *event-queue-mutex*
      :accessor mutex)
    (priority :initform :normal
      :accessor priority :documentation "The priority of the queue,
either :high or :normal"))
  (:documentation "A priority queue is a queue that supports two priorities,
 high and normal. Events enqueued with the high priority are dequeued before
events enqueued with the normal priority."))

(defmethod enqueue ((queue priority-queue) event)
  "Enqueue an event to the priority-queue
    QUEUE - the priority queue to enqueue the event to
    EVENT - the event to enqueue"
  (with-lock-held (mutex queue)
    (push event (gethash (priority queue) (queue queue)))))
(defmethod dequeue ((queue priority-queue))
  "Dequeue the next event from the priority-queue
    QUEUE - the priority queue to dequeue the event from"
  (with-lock-held (mutex queue)
    (pop (gethash (priority queue) (queue queue)))))


(defstruct event-queue
  events
  (mutex (make-instance 'clevelib.synchronization:event-mutex))
  (priority :normal))


(defun enqueue (event-queue event)
  "Enqueue an event to the event-queue
    EVENT-QUEUE - the event queue to enqueue the event to
    EVENT - the event to enqueue"
  (with-event-mutex (event-queue-mutex event-queue)
    (push event (event-queue-events event-queue))))


(defun dequeue (event-queue)
  "Dequeue the next event from the event-queue
    EVENT-QUEUE - the event queue to dequeue the event from"
  (with-event-mutex (event-queue-mutex event-queue)
    (pop (event-queue-events event-queue))))

(defun change-event-priority (event-queue priority)
  "Change the priority of an event in the event-queue
    EVENT-QUEUE - the event queue containing the event
    PRIORITY - the new priority of the event"
  (setf (event-queue-priority event-queue) priority))


;; Additional utility functions for event queue management
(defun event-queue-empty-p (event-queue)
  "Return true if the event-queue is empty, false otherwise
    EVENT-QUEUE - the event queue to check"
  (null (event-queue-events event-queue)))

(defun event-queue-length (event-queue)
  "Return the number of events in the event-queue
    EVENT-QUEUE - the event queue to check"
  (length (event-queue-events event-queue)))
