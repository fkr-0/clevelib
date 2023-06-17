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
    :destroy-all-threads
    :thread-alive-p
    :create-thread
    :make-mutex
    :thread-pool
    :get-thread
    :with-mutex
    :make-condition-variable
    :wait-on-condition
    :signal-condition
    :broadcast-condition
    :cleanup)
  (:import-from :cl.state :*state*) )

(in-package :clevelib.threads)

(defun make-mutex (&optional name)
  "Create a new mutex with the optional NAME."
  (bt:make-lock name))

(defun make-condition-variable ()
  "Create a new condition variable."
  (sb-thread:make-waitqueue))
(defclass thread-pool ()
  ((threads :initarg :threads
     :accessor threads
     :initform (make-hash-table))
    (mutex :initarg :mutex
      :accessor mutex
      :initform (make-mutex))
    (conditionv :initarg :condition
      :accessor conditionv
      :initform (make-condition-variable)))
  (:documentation "A thread pool that manages thread creation, synchronization, and cleanup."))

(defun thread-alive-p ( thread)
  "Return T if THREAD is alive, NIL otherwise."
  (bt:thread-alive-p thread))


(defmethod initialize-instance :after ((pool thread-pool) &key)
  "Initialize a thread pool."
  ;; (declare (ignorable key))
  (setf (threads pool) (make-hash-table)
    (mutex pool) (make-mutex)
    (conditionv pool) (make-condition-variable)))

(defparameter *threads* (make-instance 'thread-pool)
  "A hash table that stores active threads by their thread IDs.")

(defun get-thread (name)
  "Get a thread from the thread pool by its ID."
  (gethash name (threads *threads*)))

(defun create-thread (function  &rest args)
  "Create and start a new thread, executing FUNCTION with ARGS."
  (let ((thread (bt:make-thread (lambda () (apply function args)))))
    (setf (gethash (bt:thread-name thread) (threads *threads*)) thread)
    thread))

(defun destroy (thread )
  "Destroy a thread and remove it from the active threads hash table."
  (remhash (bt:thread-name thread) (threads *threads*))
  (bordeaux-threads:interrupt-thread  thread (lambda () (sb-thread:join-thread  thread ))))

(defun hash-table-values (hash-table)
  "Return a list of the values in HASH-TABLE."
  (loop for key being the hash-keys of hash-table
    collect (gethash key hash-table)))
(defun destroy-all-threads ()
  "Destroy all active threads."
  (bt:with-lock-held ((mutex *threads*))
    (mapc #'destroy (hash-table-values
                      (threads *threads*)))))

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

(defun cleanup ()
  "Perform cleanup tasks, such as destroying all threads."
  (destroy-all-threads ))



;; (defmethod destroy ((pool thread-pool))
;;   "Destroy a thread and remove it from the active threads hash table."
;;   (with-mutex (mutex pool)
;;     (destroy-all-threads (threads pool))))

(defmethod poolcreate-thread ((pool thread-pool) function &rest args)
  "Create and start a new thread, executing FUNCTION with ARGS."
  (with-mutex (mutex pool)
    (let ((thread (create-thread function (threads pool) args)))
      (setf (gethash (bt:thread-name thread) (threads pool)) thread)
      thread)))

(defmethod pooldestroy-thread ((pool thread-pool) thread)
  "Destroy a thread and remove it from the active threads hash table."
  (with-mutex (mutex pool)
    (destroy thread )))

(defmethod pooldestroy-all-threads ((pool thread-pool))
  "Destroy all active threads."
  (with-mutex (mutex pool)
    (destroy-all-threads )))

(defmethod poolwait-on-condition ((pool thread-pool) condition &optional timeout)
  "Wait for the CONDITION variable while releasing the MUTEX.
   Optionally, provide a TIMEOUT in seconds."
  (with-mutex (mutex pool)
    (wait-on-condition condition (mutex pool) timeout)))

(defmethod poolsignal-condition ((pool thread-pool) condition)
  (with-mutex (mutex pool)
    (signal-condition condition)))

(defmethod poolbroadcast-condition ((pool thread-pool) condition)
  (with-mutex (mutex pool)
    (broadcast-condition condition)))

(defmethod poolcleanup ((pool thread-pool))
  (with-mutex (mutex pool)
    (cleanup )))
