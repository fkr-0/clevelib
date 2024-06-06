
(in-package :clevelib-tests)
(def-suite threads-test-suite
  :description "Test suite for the clevelib.threads package"
  :in testmain)

(format t "~&~%Running tests for clevelib.threads package~%~%")
(in-suite threads-test-suite)
(use-package :clevelib.thread)
(use-package :clevelib.mutex)
(use-package :clevelib.condition-variable)

(test mutex-locking-thread-tests
  "Test mutex locking and unlocking."
  (let ((test-mutex (clevelib.mutex:make-mutex)))
    (is (typep test-mutex 'bt:lock))
    (clevelib.mutex:with-mutex test-mutex
      (is (sb-thread:holding-mutex-p test-mutex)))))

;; (test condition-variable-thread-tests
;;   "Test creating and signaling a condition variable."
;;   (let ((threads (make-hash-table :test 'equal))
;;          (test-mutex (clevelib.mutex:make-mutex))
;;          (test-condition (clevelib.cv:make-condition-variable)))
;;     (is (typep test-condition 'sb-thread:waitqueue))
;;     (loop for i from 1 to 10 do
;;       (let ((thread (clevelib.thread:create-thread (lambda ()
;;                                                      (clevelib.mutex:with-mutex test-mutex
;;                                                        (clevelib.cv:wait-on-condition test-condition test-mutex 2)
;;                                                        (format t "Thread ~A done~%" i))))))
;;         (setf (gethash i threads) thread)))
;;     (sleep 1)
;;     (clevelib.cv:signal-condition test-condition)
;;     (sleep 0.1)
;;     (is (not (sb-thread::thread-alive-p (gethash 1 threads))) "Thread should be dead after condition signal")
;;     (loop for i from 2 to 10 do
;;       (is (not (sb-thread:thread-alive-p (gethash i threads))) "Thread should be dead after condition signal"))
;;     )
;;   )

;; (test cleanup-thread-tests
;;   "Test cleanup function."
;;   (let* ((test-thread (clevelib.thread:create-thread (lambda () (loop (sleep 0.5)))))
;;           (test-thread-name (bt:thread-name test-thread)))
;;     (is (typep test-thread 'bt:thread))
;;     ;; (is (clevelib.thread:get-thread (bt:thread-name test-thread)))
;;     ;; (clevelib.thread:cleanup)
;;     (clevelib.thread:destroy test-thread)
;;     ;; (is (null  (clevelib.thread:get-thread test-thread-name) ))
;;     ))

;; ;;; Define the test suite
;; (defpackage :thread-pool-tests
;;   (:use :common-lisp :fiveam :bordeaux-threads :sb-thread :clevelib.queue))

;; (in-package :thread-pool-tests)

;; (def-suite thread-pool-suite
;;   :description "Unit tests for the thread pool and task worker functionality."
;;   :in thread-pool)
(test test-thread-pool
  "Unit tests for the thread pool and task worker functionality."
  (let ((pool (clevelib.thread-pool:make-thread-pool :thread-limit 5)))
    ;; Test thread creation and execution
    (is (eql 0 (clevelib.thread-pool:thread-count pool)))
    (let ((thread (start-thread pool (lambda () (sleep 1)))))
      (is (eql 1 (clevelib.thread-pool:thread-count pool)))
      (is (clevelib.thread-pool:thread-running-p pool thread))
      (join-pool-threads pool))
    (is (eql 0 (clevelib.thread-pool:thread-count pool)))))

;; Test task worker
(test worker
  (let* ((pool (clevelib.thread-pool:make-thread-pool :thread-limit 5))
          (worker (clevelib.thread-pool:spawn-task-worker pool :description "Test Worker")))
    (is (eql 1 (clevelib.thread-pool:thread-count pool)))
    (clevelib.thread-pool:spawn-task pool (lambda () (format t "Task executed~% ~A ~%" worker) ))
    ;; (stop-task-worker pool worker)
    ;; (join-pool-threads pool)
    (sleep 0.1)
    (is (eql 1 (clevelib.thread-pool:worker-task-count worker)))
    (clevelib.thread-pool:stop-task-worker pool worker)
    (clevelib.thread-pool:stop-all-task-workers pool)
    (is (eql 0 (clevelib.thread-pool:thread-count pool)))))
;;; Test thread creation and destruction
;; (test thread-creation-test
;;   (let ((thread (create-thread (lambda () (sleep 1)))))
;;     (is (thread-alive-p thread) "Thread should be alive initially")
;;     (bt:join-thread thread)
;;     (is (not (thread-alive-p thread)) "Thread should be dead after joining")))

;;; Test mutex and condition variables
(test mutex-and-condition-test
  (let ((test-mutex (clevelib.mutex:make-mutex))
         (condition (clevelib.cv:make-condition-variable))
         (flag nil))
    (bt:make-thread (lambda ()
                      (clevelib.mutex:with-mutex test-mutex
                        (clevelib.cv:wait-on-condition condition test-mutex 2)
                        (setf flag t))))
    (sleep 0.5)
    (clevelib.cv:signal-condition condition)
    (sleep 0.5)
    (format t "Flag: ~A~%" flag)
    (is (not (null flag)) "Flag should be set after condition signal")))

;;; Test thread pool creation and basic operations
(test thread-pool-creation-test
  (let ((pool (clevelib.thread-pool:make-thread-pool :thread-limit 2)))
    (is (eql 0 (clevelib.thread-pool:thread-count pool)) "Initial thread count should be 0")
    (let ((thread (start-thread pool (lambda () (sleep 1)))))
      (is (eql 1 (clevelib.thread-pool:thread-count pool)) "Thread count should be 1 after starting a thread")
      (is (clevelib.thread-pool:thread-running-p pool thread) "Thread should be running")
      (join-pool-threads pool)
      (is (eql 0 (clevelib.thread-pool:thread-count pool)) "Thread count should be 0 after joining all threads"))))

;;; Test adding and executing tasks in the thread pool
(test thread-pool-task-test
  (let ((pool (clevelib.thread-pool:make-thread-pool :thread-limit 2)))
    (let ((test-result nil))
      (clevelib.thread-pool:spawn-task pool (lambda ()

                                              (setf test-result :done)
                                              (format t "Task executed ~A ~%" test-result)
                                              (sleep 3)))
      (let ((worker (clevelib.thread-pool:spawn-task-worker pool :description "Task Worker")))
        (sleep 1)
        (clevelib.thread-pool:stop-task-worker pool worker)
        (is (eql :done test-result) "Task should be executed by worker")
        ;; (format t "Worker stopped ~A ~A ~%" pool result)
        (is (eql 0 (clevelib.thread-pool:thread-count pool)) "Thread count should be 0 after stopping worker")))))
;; (defvar zz (clevelib.thread-pool:make-thread-pool :thread-limit 2))
;; (defvar result nil)
;; (clevelib.thread:spawn-task zz (lambda ()
;;                                          (format t "Task executed~%")
;;                                          (setf result :done)))
;; (defvar w (clevelib.thread-pool:spawn-task-worker zz :description "Task Worker"))
;; (worker-task-count w)
;; (stop-task-worker zz w)
;; (stop-all-task-workers zz)
;; (clevelib.thread-pool:thread-count zz)
;;; Test resizing the thread pool
(test thread-pool-resize-test
  (let ((pool (clevelib.thread-pool:make-thread-pool :thread-limit 2)))
    (is (eql 2 (clevelib.thread-pool:thread-limit pool)) "Initial thread limit should be 2")
    (clevelib.thread-pool:resize-pool pool 4)
    (is (eql 4 (clevelib.thread-pool:thread-limit pool)) "Thread limit should be 4 after resizing")
    (let ((threads (mapcar (lambda (n)
                             (clevelib.thread-pool:start-thread pool (lambda () (sleep n))))
                     '(1 2 3 4))))
      (declare (ignore threads))
      (is (eql 4 (clevelib.thread-pool:thread-count pool)) "Thread count should be 4 after starting 4 threads")
      (clevelib.thread-pool:join-pool-threads pool)
      (is (eql 0 (clevelib.thread-pool:thread-count pool)) "Thread count should be 0 after joining all threads"))))
