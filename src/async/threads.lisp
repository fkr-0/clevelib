;;;; clevelib/threads.lisp
;;;;
;;;; This module provides utilities and functions for working with threads
;;;; in the event system. It manages thread creation, synchronization, and
;;;; cleanup. It also implements mutexes and condition variables for
;;;; thread-safe access to shared resources and efficient signaling between
;;;; threads.

(defpackage :clevelib.threads
  (:use :cl :bordeaux-threads)
  (:export :create-thread
    :destroy-thread
    :destroy-all-threads
    :make-mutex
    :with-mutex
    :make-condition-variable
    :wait-on-condition
    :signal-condition
    :broadcast-condition
    :cleanup))
(in-package :clevelib.threads)
;; (ql:quickload :bordeaux-threads)
;;;; Thread management

(defparameter *threads* (make-hash-table)
  "A hash table that stores active threads by their thread IDs.")

(defun create-thread (function &rest args)
  "Create and start a new thread, executing FUNCTION with ARGS."
  (let ((thread (bt:make-thread (lambda () (apply function args)))))
    (setf (gethash (bt:thread-name thread) *threads*) thread)
    thread))

(defun destroy-thread (thread)
  "Destroy a thread and remove it from the active threads hash table."
  (remhash (bt:thread-name thread) *threads*)
  (bt:destroy-thread thread))

(defun destroy-all-threads ()
  "Destroy all active threads."
  (maphash (lambda (id thread)
             (declare (ignorable id))

             (destroy-thread thread)) *threads*))

;;;; Mutexes and condition variables

(defun make-mutex (&optional name)
  "Create a new mutex with the optional NAME."
  (bt:make-lock name))

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


(defun make-condition-variable ()
  "Create a new condition variable."
  (bt:make-condition-variable))

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

;;;; Cleanup

(defun cleanup ()
  "Perform cleanup tasks, such as destroying all threads."
  (destroy-all-threads))

;;;; exported symbols
;;;; (export '(create-thread
;;;;          destroy-thread
;;;;          destroy-all-threads
;;;;          make-mutex
;;;;          with-mutex
;;;;          make-condition-variable
;;;;          wait-on-condition
;;;;          signal-condition
;;;;          broadcast-condition
;;;;          cleanup))

;;; tests
;;; (defpackage #:thread-tests
;;   (:use :cl :fiveam))

;; (in-package #:thread-tests)

;; (def-suite thread-tests :description "Test suite for thread management.")

;; (def-test create-destroy-thread thread-tests
;;   "Test creating and destroying a thread."
;;   (let ((test-thread (clevelib:create-thread (lambda () (sleep 1)))))
;;     (is (typep test-thread 'bt:thread))
;;     (is (gethash (bt:thread-name test-thread) clevelib:*threads*))
;;     (clevelib:destroy-thread test-thread)
;;     (is (null (gethash (bt:thread-name test-thread) clevelib:*threads*)))))

;; (def-test mutex-locking thread-tests
;;   "Test mutex locking and unlocking."
;;   (let ((test-mutex (clevelib:make-mutex)))
;;     (is (typep test-mutex 'bt:lock))
;;     (clevelib:with-mutex (test-mutex)
;;       (is (bt:lock-held-p test-mutex)))))

;; (def-test condition-variable thread-tests
;;   "Test creating and signaling a condition variable."
;;   (let ((test-mutex (clevelib:make-mutex))
;;         (test-condition (clevelib:make-condition-variable)))
;;     (is (typep test-condition 'bt:condition-variable))
;;     (bt:make-thread (lambda () (clevelib:wait-on-condition test-condition test-mutex 3)))
;;     (sleep 1) ;; Give other thread a chance to wait on the condition
;;     (clevelib:signal-condition test-condition)
;;     ;; No check here: if thread does not wake up, the test will hang
;;     ))
;; (def-test cleanup thread-tests
;;   "Test cleanup function."
;;   (let ((test-thread (clevelib:create-thread (lambda () (sleep 1)))))
;;     (is (typep test-thread 'bt:thread))
;;     (is (gethash (bt:thread-name test-thread) clevelib:*threads*))
;;     (clevelib:cleanup)
;;     (is (null (gethash (bt:thread-name test-thread) clevelib:*threads*)))))

;; ;; To run the tests
;; (fiveam:run! 'thread-tests)
