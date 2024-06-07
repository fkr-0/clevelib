(in-package :clevelib-tests)
(def-suite thread-pool-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain)

(in-suite thread-pool-test-suite)
(use-package :clevelib.thread-pool)

(test test-poolwait-condition
  (let ((pool (make-instance 'thread-pool))
         (condition (make-condition)))
    (is (poolwait-condition pool condition))))

(test test-poolsignal-condition
  (let ((pool (make-instance 'thread-pool))
         (condition (make-condition)))
    (is (poolsignal-condition pool condition))))

(test test-poolbroadcast-condition
  (let ((pool (make-instance 'thread-pool))
         (condition (make-condition)))
    (is (poolbroadcast-condition pool condition))))

(test test-pooladd-task
  (let ((pool (make-instance 'thread-pool))
         (task (lambda () (print "Hello, World!"))))
    (is (pooladd-task pool task))
    (is (= (queue-length (task-queue pool)) 1))))

;; Test make-thread-pool function
(test test-make-thread-pool
  (let ((pool (make-thread-pool :thread-limit 5)))
    (is (typep pool 'thread-pool))
    (is (= (thread-limit pool) 5))
    (is (= (thread-count pool) 0)))
  )

;; Test start-thread function
(test test-start-thread
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (start-thread pool (lambda () (format t "Thread 2 running~%")))
    (is (= (thread-count pool) 2))
    (is (thread-running-p pool (gethash "[P0|2]<>" (threads pool))))
    (is (thread-running-p pool (gethash "[P1|2]<>" (threads pool))))))

;; Test done function
(test test-thread-pool-done
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (start-thread pool (lambda () (format t "Thread 2 running~%")))
    (done pool "[P0|2]<>")
    (is (not (thread-running-p pool (gethash "[P0|2]<>" (threads pool)))))
    (is (= (thread-count pool) 1))))

;; Test resize function
(test test-resize
  (let ((pool (make-thread-pool :thread-limit 2)))
    (resize-pool pool 5)
    (is (= (thread-limit pool) 5))
    (resize-pool pool 1)
    (is (= (thread-limit pool) 1))))

;; Test join-pool-threads function
(test test-thread-pool-join
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (start-thread pool (lambda () (format t "Thread 2 running~%")))
    (join-pool-threads pool)
    (is (= (thread-count pool) 0))))

;; Test stop-thread function

(test test-thread-pool-stop
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (start-thread pool (lambda () (format t "Thread 2 running~%")))
    (stop-thread pool (gethash "[P0|2]<>" (threads pool)))
    (is (not (thread-running-p pool (gethash "[P0|2]<>" (threads pool)))))
    (is (= (thread-count pool) 1))))

;; Test active-threads function
(test test-pool-active-threads
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (start-thread pool (lambda () (format t "Thread 2 running~%")))
    (is (equal (active-threads pool) (list "[P0|2]<>" "[P1|2]<>")))))

;; Test in-pool function
(test in-pool
  (let ((pool (make-thread-pool :thread-limit 2)))
    (start-thread pool (lambda () (format t "Thread 1 running~%")))
    (is (in-pool pool (gethash "[P0|2]<>" (threads pool))))
    (is (not (in-pool pool (gethash "[P1|2]<>" (threads pool)))))))

;; Test spawn-task function
(test test-pool-spawn-task
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task pool (lambda () (format t "Task 1 running~%")))
    (spawn-task pool (lambda () (format t "Task 2 running~%")))
    (is (not (clevelib.queue:queue-empty-p (task-queue pool))))))

;; Test spawn-task-worker function
(test test-pool-spawn-task-worker
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (spawn-task-worker pool)
    (is (= (thread-count pool) 2))
    (is (thread-running-p pool (gethash "QueueWorker[P0|2]<>" (threads pool))))
    (is (thread-running-p pool (gethash "QueueWorker[P1|2]<>" (threads pool))))))

;; Test stop-task-worker function
(test stop-task-worker
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (spawn-task-worker pool)
    (stop-task-worker pool (gethash "QueueWorker[P0|2]<>" (threads pool)))
    (is (not (thread-running-p pool (gethash "QueueWorker[P0|2]<>" (threads pool)))))
    (is (= (thread-count pool) 1))))

;; Test stop-all-task-workers function
(test stop-all-task-workers
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (spawn-task-worker pool)
    (stop-all-task-workers pool)
    (is (= (thread-count pool) 0))))

;; Test worker-p function
(test worker-p-fun
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (is (worker-p pool (gethash "QueueWorker[P0|2]<>" (threads pool))))
    (is (not (worker-p pool (gethash "[P0|2]<>" (threads pool)))))))

;; Test waiting-task-workers function
(test waiting-task-workers
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (spawn-task-worker pool)
    (is (equal (waiting-task-workers pool) (list (gethash "QueueWorker[P0|2]<>" (threads pool)) (gethash "QueueWorker[P1|2]<>" (threads pool)))))))

;; Test query-next function
(test query-next
  (let ((pool (make-thread-pool :thread-limit 2)))
    (spawn-task-worker pool)
    (let ((worker (gethash "QueueWorker[P0|2]<>" (threads pool))))
      (is (null (query-next pool worker)))
      (setf (worker-on-finish worker) :finish)
      (is (eq :finish (query-next pool worker)))
      (setf (worker-on-finish worker) :wait)
      (is (eq :wait (query-next pool worker)))
      (setf (worker-on-finish worker) :query)
      (is (eq :query (query-next pool worker))))))
