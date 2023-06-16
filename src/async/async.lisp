;;;; clevelib/async.lisp
;;;;
;;;; This module handles asynchronous execution, including running tasks
;;;; asynchronously and managing their state.

;; (in-package :clevelib)
;; Import symbols from threads module
;; (import '(clevelib:make-thread
;;           clevelib:thread-join
;;           clevelib:make-mutex
;;           clevelib:with-mutex
;;           clevelib:make-condition-variable
;;           clevelib:condition-variable-wait
;;           clevelib:condition-variable-signal))
(ql:quickload :bordeaux-threads)
;; Define a struct to represent an async task
(defstruct (async-task (:conc-name async-task-))
  function
  args
  thread
  lock
  result
  error
  finished)


(defun make-async-task (function &rest args)
  "Create an async task. On execution a thread is created
    and the function is executed in that thread. The result of the function is
    stored in the task.
   Returns the task itself."
  (make-async-task :function function
    :args args
    :lock (bt:make-recursive-lock)
    :finished nil))
                                        ; Initialize status as :pending

(defun async-exec (task)
  "Execute the given async task. Returns the task itself.
   The task will be executed in a new thread, and the result will be stored in
   the task. If the task is already finished, this function does nothing."
  (unless (async-task-finished task)
    (setf (async-task-thread task)
      (bt:make-thread
        (lambda ()
          (handler-case
            (progn
              (loop while (bt:with-lock-held ((async-task-lock task))
                            (not (async-task-finished task)))
                do (bt:with-lock-held ((async-task-lock task))
                     (setf (async-task-result task)
                       (apply (async-task-function task)
                         (async-task-args task)))
                     (setf (async-task-finished task) t)))
              (error (err)
                (bt:with-lock-held ((async-task-lock task))
                  (setf (async-task-error task) err)
                  (setf (async-task-finished task) t))))))))))



(defun cancel-async-task (task)
  "Cancel the given async task, if possible."
  (unless (async-task-finished task)
    (when (bt:thread-alive-p (async-task-thread task))
      (bt:terminate-thread (async-task-thread task)))
    (bt:with-lock-held ((async-task-lock task))
      (setf (async-task-finished task) t))))


(defun async-task-finished-p (task)
  "Return true if the given async task has finished."
  (with-mutex ((async-task-mutex task))
    (async-task-finished task)))

;; Usage example
;; (let ((task (make-async-task #'sleep 5)))
;;  (async-exec task)
;;  (format t "Task finished: ~a~%" (async-task-result task :timeout 10)))
;;
;;
(defun async-task-result (task &key (timeout 0))
  "Get the result of the given async task, waiting for up to `timeout` seconds.
   Returns multiple values: the result, error (if any), and a boolean indicating if the task is finished."
  (bt:with-lock-held ((async-task-mutex task))
    (when (not (async-task-finished task))
      (bt:condition-wait (async-task-condition-variable task)
        (async-task-mutex task)
        :timeout timeout))
    (cond ((eq (async-task-status task) :error)
            (error (format nil "Task finished with error: ~A" (async-task-error task))))
      (t
        (values (async-task-result task)
          (async-task-error task)
          (async-task-finished task))))))



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
