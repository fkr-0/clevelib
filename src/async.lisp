;;;; clevelib/async.lisp
;;;;
;;;; This module handles asynchronous execution, including running tasks
;;;; asynchronously and managing their state.

(uiop:define-package :clevelib.async
  (:use :cl :log4cl)
  (:import-from :clevelib.mutex
    :make-mutex
    :with-mutex)
  (:import-from :clevelib.cv
    :make-condition-variable
    :wait-on-condition
    :signal-condition)
  (:import-from :clevelib.thread
    :destroy
    :start-thread
    :thread-alive-p
    :create-thread
    :set-thread-name)
  (:export
    :async-task
    :new-task
    :make-async-task
    :async-exec
    :cancel-async-task
    :async-run
    :async-task-finished-p
    :async-task-result
    :async-task-status
    :async-task-error
    :sync-run
    :async-exec
    :poll-result
    :cancel-async-task
    :async-task-finished-p
    :promise
    :resolve-promise
    :reject-promise
    :await))
(in-package :clevelib.async)

(defstruct (promise (:conc-name promise-))
  value
  error
  resolved
  condition
  lock)

(defmethod initialize-instance :after ((p promise) &key)
  (setf (promise-lock p) (make-mutex))
  (setf (promise-condition p) (make-condition-variable)))

(defstruct (async-task (:conc-name async-task-))
  function
  args
  thread
  status
  condition-variable
  lock
  result
  error
  promise
  finished)


(defmethod initialize-instance :after ((task async-task) &key
                                        (function nil)
                                        (args nil)
                                        (promise nil))
  "This method is used to initialize aninstance of the `async-task` class. It sets
up the necessary properties for the task, including the function to be executed,
theargumentstopasstothefunction,thepromiseassociatedwiththetask,and various
status flags. The method also creates a mutex and condition variable to be used
for synchronizing the execution of the task.
  Args:
    task(async-task): The task instance to initialize.
    function(function): The function to execute asynchronously.
    args(list): The arguments to pass to the function.
    promise(promise): The promise associated with the task.
  Returns:
    None."
  (setf
    (async-task-promise task)
    (or promise
      (make-instance 'promise)))
  (when function
    (setf
      (async-task-function task) function))
  (when args
    (setf
      (async-task-args task) args))
  (setf
    (async-task-status task) :pending)
  (setf
    (async-task-lock task)
    (make-mutex))
  (setf
    (async-task-condition-variable task)
    (make-condition-variable))
  (setf
    (async-task-finished task) nil)
  (setf
    (async-task-result task) nil)
  (setf
    (async-task-error task) nil))

(defun new-task (function &rest args)
  "Create a new async task with the given function and arguments."
  (make-instance 'async-task :function function :args args))

(defmethod sync-run ((task async-task))
  "Run the given function synchronously, with the given arguments.
   Returns the result of the function. If the function throws an error, the
   error will be returned instead."
  (handler-case
    (progn
      (setf (async-task-status task) :running)
      (setf (async-task-result task)
        (apply (async-task-function task)
          (async-task-args task)))
      (setf (async-task-status task) :completed)
      (setf (async-task-finished task) t)
      (resolve-promise (async-task-promise task) (async-task-result task))
      (signal-condition (async-task-condition-variable task)))
    (error (err)
      (setf (async-task-status task) :error)
      (setf (async-task-error task) err)
      (setf (async-task-finished task) t)
      (reject-promise (async-task-promise task) err)
      (signal-condition (async-task-condition-variable task))
      (error err))));;for now

(defgeneric spawn-task (pool task))
(defmethod async-exec ((task async-task) &optional (pool nil))
  "Execute the given async task. Returns the task itself.
   The task will be executed in a new thread, and the result will be stored in
   the task. If the task is already finished, this function does nothing."
  (unless (async-task-finished task)
    (if pool
      (spawn-task pool task)
      (setf (async-task-thread task)
        (clevelib.thread:create-thread
          (lambda ()
            (sync-run task)))))
    task))

;; (handler-case
;;   (progn
;;     (loop while (clevelib.thread:with-mutex (async-task-lock task)
;;                   (not (async-task-finished task)))
;;       do (clevelib.thread:with-mutex (async-task-lock task)
;;            (setf (async-task-result task)
;;              (apply (async-task-function task)
;;                (async-task-args task)))
;;            (setf (async-task-finished task) t))))
;;   (error (err)
;;     (clevelib.thread:with-mutex (async-task-lock task)
;;       (setf (async-task-status task) :error)
;;       (setf (async-task-error task) err)
;;       (setf (async-task-finished task) t)))


(defun poll-result (task &key (timeout 0))
  "Get the result of the given async task, waiting for up to `timeout` seconds.
   Returns multiple values: the result, error (if any), and a boolean indicating
   if the task is finished. If the task is not finished, the result and error
   will be nil.
   If the task is finished with an error, this function will throw an error.
  Args:
    task(async-task): The async task to get the result of.
    timeout(number): The maximum time to wait for the task to finish."
  (with-mutex (async-task-lock task)
    (when (not (async-task-finished task))
      (wait-on-condition
        (async-task-condition-variable task)
        (async-task-lock task)
        timeout))
    (cond ((eq (async-task-status task) :error)
            (error (format nil "Task finished with error: ~A"
                     (async-task-error task))))
      (t (values (async-task-result task)
           (async-task-error task)
           (async-task-finished task))))))

(defun cancel-async-task (task)
  "Cancel the given async task, if possible."
  (unless (async-task-finished task)
    (when (clevelib.thread:thread-alive-p (async-task-thread task))
      (clevelib.thread:destroy (async-task-thread task) ))
    (with-mutex (async-task-lock task)
      (setf (async-task-finished task) t))))

(defun async-task-finished-p (task)
  "Return true if the given async task has finished.
   This function does not block."
  (with-mutex (async-task-lock task)
    (async-task-finished task)))


(defun resolve-promise (p value)
  (with-mutex (promise-lock p)
    (setf (promise-value p) value)
    (setf (promise-resolved p) t)
    (signal-condition (promise-condition p))))

(defun reject-promise (p error)
  (with-mutex (promise-lock p)
    (setf (promise-error p) error)
    (setf (promise-resolved p) t)
    (signal-condition (promise-condition p))))


(defmethod await ((p promise) &optional (timeout 0))
  (with-mutex (promise-lock p)
    (log-info "awaiting")
    (loop until (promise-resolved p)
      do (progn (log-info "stillawaiting ")
           (wait-on-condition (promise-condition p) (promise-lock p) timeout)))
    (if (promise-error p)
      (progn (log-info "ERRR RESOLVE") (error (promise-error p)))
      (progn (log-info "succ resolve" (promise-value p))(promise-value p)))))

(defmethod await ((task async-task) &optional (timeout 0))
  (await (async-task-promise task) timeout))


;;;; Example usage
;; First, let's define a function that we want to execute asynchronously.
;; (defun slow-function (n)
;;   (sleep n)  ;; Sleep for n seconds to simulate a slow operation.
;;   (format nil "Finished sleeping for ~a seconds." n))

;; Now let's create an async task using this function.
;; (defvar *task* (make-async-task #'slow-function 5))

;; ;; We can start the task using async-exec.
;; (async-exec *task*)

;; ;; At this point, the slow-function is running in a separate thread. We can
;; ;; continue to do other things in the main thread. Let's just wait for a bit.
;; (sleep 1)

;; ;; Now let's check if the task is finished.
;; (format t "Task finished: ~a~%" (async-task-finished-p *task*))

;; ;; It's not finished yet, because slow-function sleeps for 5 seconds and we
;; ;; only waited for 1 second. Let's get the result of the task, waiting up to
;; ;; 10 seconds for it to finish.
;; (multiple-value-bind (result error finished) (async-task-result *task* :timeout 10)
;;   (if (eq finished :error)
;;       (format t "Task finished with error: ~a~%" error)
;;       (format t "Task finished: ~a~%" result)))

;; ;; The task should be finished now, and we should see the result of the
;; ;; slow-function.

;;; Tests
;;; ;; Ensure the fiveam library is loaded
;; (ql:quickload :fiveam)

;; ;; Import the fiveam package for testing
;; (use-package :fiveam)

;; ;; Define the test suite
;; (def-suite async-task-tests :description "Tests for the async-task system")

;; ;; Define a test to check async-task creation
;; (def-test make-async-task-test async-task-tests
;;   (let* ((task-fn (lambda (n) (* n n)))
;;          (task (make-async-task task-fn 5)))
;;     (is (equal (async-task-function task) task-fn))
;;     (is (equal (async-task-args task) '(5)))
;;     (is (equal (async-task-finished task) nil))))

;; ;; Define a test to check async-task execution and result retrieval
;; (def-test async-exec-and-result-test async-task-tests
;;   (let* ((task-fn (lambda (n) (* n n)))
;;          (task (make-async-task task-fn 5)))
;;     (async-exec task)
;;     (sleep 1)  ;; Give the task some time to complete
;;     (multiple-value-bind (result error finished) (async-task-result task :timeout 10)
;;       (is (equal result 25))
;;       (is (equal error nil))
;;       (is (equal finished t)))))

;; ;; Define a test to check async-task cancellation
;; (def-test cancel-async-task-test async-task-tests
;;   (let* ((task-fn (lambda (n) (sleep n) (* n n)))
;;          (task (make-async-task task-fn 10)))
;;     (async-exec task)
;;     (cancel-async-task task)
;;     (sleep 1)  ;; Give the task some time to be cancelled
;;     (is (equal (async-task-finished task) t))))

;; ;; Run the test suite
;; (fiveam:run! 'async-task-tests)


