(defpackage :clevelib.queue
  (:use :cl :bt)
  (:export :priority-queue
    :queue-empty-p
    :enqueue
    :dequeue-wait
    :make-queue
    :queue-lock
    :item-added
    :counter
    :dequeue
    :dequeue-no-wait
    :queue-length))

(in-package :clevelib.queue)

(defclass priority-queue ()
  ((normal-items :accessor normal-items :initform nil)
    (high-items :accessor high-items :initform nil)
    (counter :accessor counter :initform 0)
    (lock :accessor queue-lock :initform (bt:make-lock))
    (item-added  :accessor item-added :initform (bt:make-condition-variable)))
  (:documentation "A priority queue."))

(defun make-queue ()
  "Create a new priority queue."
  (make-instance 'priority-queue))

(defmethod enqueue ((queue priority-queue) item &optional (priority :normal))
  "Add an item to the queue. If priority is :normal, the item is added to the
   normal priority queue. If priority is :high, the item is added to the high
   priority queue."
  (bt:with-lock-held ((queue-lock queue))
    (let ((q (case priority
               (:normal (normal-items queue))
               (:high (high-items queue))))
           (h (eql priority :high)))
      (push item q)
      (if h (setf (high-items queue) q)
        (setf (normal-items queue) q))
      (incf (counter queue))
      (bt:condition-notify (item-added queue)))))

(defmethod dequeue-wait ((queue priority-queue))
  "Wait until an item is available."
  (bt:with-lock-held ((queue-lock queue))
    (loop
      while (and (null (high-items queue))
              (null (normal-items queue)))
      do (bt:condition-wait (item-added queue) (queue-lock queue)))
    (decf (counter queue))
    (if (high-items queue)
      (pop (high-items queue))
      (pop (normal-items queue)))))

(defmethod dequeue-no-wait ((queue priority-queue))
  "Return nil if no item is available."
  (bt:with-lock-held ((queue-lock queue))
    (when (or (high-items queue) (normal-items queue))
      (decf (counter queue))
      (if (high-items queue)
        (pop (high-items queue))
        (pop (normal-items queue))))))

(defmethod dequeue ((queue priority-queue) &optional (wait t))
  "If wait is true, wait until an item is available. If wait is false, return
   nil if no item is available."
  (if wait
    (dequeue-wait queue)
    (dequeue-no-wait queue)))

(defmethod queue-length ((queue priority-queue))
  "Return the number of items in the queue."
  (bt:with-lock-held ((queue-lock queue))
    (counter queue)))

(defmethod queue-empty-p ((queue priority-queue))
  "Return true if the queue is empty."
  (zerop (queue-length queue)))
