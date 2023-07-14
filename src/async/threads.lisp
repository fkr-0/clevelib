;;;; clevelib/threads.lisp
;;;;
;;;; This module provides utilities and functions for working with threads
;;;; in the event system. It manages thread creation, synchronization, and
;;;; cleanup. It also implements mutexes and condition variables for
;;;; thread-safe access to shared resources and efficient signaling between
;;;; threads.

(defpackage :clevelib.threads
  (:use :cl :bordeaux-threads)
  (:export
    :destroy
    :start-thread
    :destroy-all-threads
    :thread-alive-p
    :create-thread
    :set-thread-name
    :make-thread-pool
    :make-mutex
    :thread-pool
    :get-thread
    :with-mutex
    :make-condition-variable
    :wait-on-condition
    :signal-condition
    :broadcast-condition
    :cleanup)
  )

(in-package :clevelib.threads)

;;; Primitives

(defun make-mutex (&optional name)
  "Create a new mutex with the optional NAME."
  (bt:make-lock name))

(defun make-condition-variable ()
  "Create a new condition variable."
  (sb-thread:make-waitqueue))

(defun thread-alive-p ( thread)
  "Return T if THREAD is alive, NIL otherwise."
  (bt:thread-alive-p thread))

(defun set-thread-name (thread name)
  "Set the name of THREAD to NAME."
  (setf (slot-value thread 'sb-thread::name ) name))




;; (defparameter *threads* (make-instance 'thread-pool)
;;   "A hash table that stores active threads by their thread IDs.")


(defun create-thread (function  &rest args)
  "Create and start a new thread, executing FUNCTION with ARGS."
  (let ((thread (bt:make-thread (lambda () (apply function args)))))

    thread))


(defun destroy (thread )
  "Destroy a thread and remove it from the active threads hash table."

  (bordeaux-threads:interrupt-thread
    thread (lambda () (sb-thread:join-thread
                        (sb-thread:main-thread)))))

(defun hash-table-values (hash-table)
  "Return a list of the values in HASH-TABLE."
  (loop for key being the hash-keys of hash-table
    collect (gethash key hash-table)))

;; (maphash (lambda (id thread)
;;            (declare (ignorable id))

;;            (destroy thread )) (threads *threads*))
;; (setf (threads *threads*) (make-hash-table)))

;;;; Mutexes and condition variables

;; (bt:defcondition condition-variable-error (error)
;;   ()
;;   (:report (lambda (condition stream)
;;              (format stream "Condition variable error: ~A" condition))))

(defmacro with-mutex (mutex &body body)
  "Execute BODY while holding the MUTEX."
  `(bt:with-lock-held (,mutex)
     (progn
       ,@body)))

;; (defun with-mutex (mutex &rest body)
;;   "Execute BODY while holding the MUTEX."
;;   (bt:with-lock-held (mutex)
;;     (progn
;;       (apply body))))


(defun wait-on-condition (condition mutex &optional timeout)
  "Wait for the CONDITION variable while releasing the MUTEX.
   Optionally, provide a TIMEOUT in seconds."
  (bt:with-timeout (timeout)
    (bt:condition-wait condition mutex)))

(defun signal-condition (condition)
  "Signal the CONDITION variable to wake up a single waiting thread."
  (bt:condition-notify condition))

(defun broadcast-condition (condition)
  "Signal the CONDITION variable to wake up all waiting threads."
  (sb-thread:condition-broadcast condition))





;; (defmethod destroy ((pool thread-pool))
;;   "Destroy a thread and remove it from the active threads hash table."
;;   (with-mutex (mutex pool)
;;     (destroy-all-threads (threads pool))))

