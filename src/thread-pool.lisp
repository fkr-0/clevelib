(uiop:define-package :clevelib.thread-pool
  (:use :cl )
  (:import-from :bordeaux-threads
    :join-thread
    :thread-alive-p)
  (:import-from :clevelib.queue
    :make-queue
    :queue-length
    :enqueue
    :dequeue
    :queue-empty-p)
  (:import-from :clevelib.mutex
    :with-mutex
    :make-mutex)
  (:import-from :clevelib.thread
    :destroy
    :create-thread)
  (:import-from :clevelib.condition-variable
    :make-condition-variable
    :wait-on-condition
    :signal-condition
    :broadcast-condition)
  (:import-from :clevelib.async
    :make-async-task
    :sync-run
    :async-run
    :new-task
    :async-task
    :spawn-task
    :make-promise)
  (:export
    :active-threads
    :done
    :finish-task
    :in-pool
    :join-pool-threads
    :make-thread-pool
    :pooladd-task
    :poolbroadcast-condition
    :poolcreate-thread
    :pooldestroy-thread
    :poolget-task
    :poolsignal-condition
    :poolwait-on-condition
    :query-next
    :resize-pool
    :spawn-task
    :spawn-task-worker
    :start-thread
    :stop-all-task-workers
    :stop-task-worker
    :stop-thread
    :task-queue
    :task-worker
    :task-worker-error
    :thread-count
    :destroy-all-threads
    :join-pool-threads
    :threads
    :thread-limit
    :thread-pool
    :thread-running-p
    :waiting-task-workers
    :worker-on-finish
    :worker-p
    :worker-task-count))

(in-package :clevelib.thread-pool)




(defclass thread-pool ()
  ((threads :initarg :threads
     :accessor threads
     :initform (make-hash-table)
     :documentation "A hash table of threads in the pool.")
    (mutex :initarg :mutex
      :accessor mutex
      :initform (make-mutex)
      :documentation "The mutex for the thread pool. This is used to synchronize access to the thread pool.")
    (conditionv :initarg :condition
      :accessor conditionv
      :initform (make-condition-variable)
      :documentation "The condition variable for the thread pool. This is used to signal when a thread is available.")
    (workers :initarg :workers
      :accessor pool-workers
      :initform nil
      :documentation "The queue of workers to be executed by the thread pool. This is used to synchronize access to the thread pool.")
    (task-queue :initarg :task-queue
      :accessor task-queue
      :initform (clevelib.queue:make-queue)
      :documentation "The queue of tasks to be executed by the thread pool. This is used to synchronize access to the thread pool.
And to signal when a thread is available. When a thread is available, it will
pop a task off the queue and execute it. If the queue is empty, the thread will
wait on the condition variable.")
    ;; (thread-count :initarg :thread-count
    ;;   :accessor thread-count
    ;;   :initform 0 :documentation "The number of threads in the pool.")
    (thread-limit :initarg :thread-limit :accessor thread-limit :initform 0
      :documentation "The maximum number of threads allowed in the pool. 0 means no limit."))
  (:documentation "A thread pool that manages thread creation, synchronization, and cleanup."))
(defstruct (task-worker (:conc-name worker-))
  (thread nil :type (or null bt:thread))
  (task nil :type (or null async-task))
  (pool nil :type thread-pool)
  (description nil :type string)
  (task-count 0 :type fixnum)
  (state :init :type (member :init :waiting :running :done :error))
  (err-state nil :type (or null condition))
  (err-message "" :type string)
  (on-finish :wait :type (member :wait :query :finish)))

(defmethod print-object ((pool thread-pool) stream)
  (print-unreadable-object (pool stream :type t :identity t)
    (format stream "[Thrds:~A;Limit:~A;TQ:~A]"
      (thread-count pool) (thread-limit pool)
      (clevelib.queue:queue-length (task-queue pool)))))

;;; Initialization

(defmethod initialize-instance :after ((pool thread-pool) &key)
  "Initialize a thread pool.
   This method is called after the thread pool is initialized.
   It initializes the thread pool's mutex and condition variable."
  ;; (declare (ignorable key))
  (setf (threads pool) (make-hash-table)
    (mutex pool) (make-mutex)
    (conditionv pool) (make-condition-variable)))

;;; Primitives
(defmethod poolcreate-thread ((pool thread-pool) function &rest args)
  "Create and start a new thread, executing FUNCTION with ARGS.
   Return the thread object."
  (with-mutex (mutex pool)
    (let ((thread (create-thread function (threads pool) args)))
      (setf (gethash (bt:thread-name thread) (threads pool)) thread)
      thread)))

(defmethod pooldestroy-thread ((pool thread-pool) thread)
  "Destroy a thread and remove it from the active threads hash table.
   Return the thread object."
  (with-mutex (mutex pool)
    (destroy thread )))

(defmethod poolwait-on-condition ((pool thread-pool) condition &optional timeout)
  "Wait for the CONDITION variable while releasing the MUTEX.
   Optionally, provide a TIMEOUT in seconds."
  (with-mutex (mutex pool)
    (wait-on-condition condition (mutex pool) timeout)))

(defmethod poolsignal-condition ((pool thread-pool) condition)
  "Signal the CONDITION variable."
  (with-mutex (mutex pool)
    (signal-condition condition)))

(defmethod poolbroadcast-condition ((pool thread-pool) condition)
  (with-mutex (mutex pool)
    (broadcast-condition condition)))

(defmethod pooladd-task ((pool thread-pool) task)
  "Add a TASK to the task queue."
  (with-mutex (mutex pool)
    (clevelib.queue:enqueue task (task-queue pool))))

(defmethod poolget-task ((pool thread-pool))
  "Get a task from the task queue."
  (with-mutex (mutex pool)
    (clevelib.queue:dequeue (task-queue pool))))

;;; API
;;;
;;; The following functions are the public API for the thread pool.
;;; They are wrappers around the primitives above.
;;;

(defun make-thread-pool (&key (thread-limit 0))
  "Create a new thread pool with THREAD-LIMIT threads."
  (make-instance 'thread-pool :thread-limit thread-limit))

(defmethod start-thread ((pool thread-pool) function &key (description t))
  "Start a new thread in the pool executing FUNCTION.
   Return the thread object."
  (when (and (< 0 (thread-limit pool)) (= (thread-count pool) (thread-limit pool)))
    (error "Thread pool is at max size"))
  (let* ((t-name (format nil "[P~d|~d]<~a>" (thread-count pool) (thread-limit pool)
                   (if (not (eq t description)) description "")))
          (thread (bt:make-thread
                    (lambda ()
                      (funcall function)
                      (done pool t-name))
                    :name t-name)))
    (setf (gethash t-name (threads pool)) thread)
    thread))

;; (defmethod pooled-task-thread ((pool thread-pool) function &key (description t))
;;   (when (and (< 0 (thread-limit pool)) (= (thread-count pool) (thread-limit pool)))
;;     (error "Thread pool is at max size"))
;;   (let* ((t-name (format nil "[P~d|~d]<~a>" (thread-count pool) (thread-limit pool)
;;                    (if (not (eq t description)) description "")))
;;           (thread (bt:make-thread
;;                     (lambda ()
;;                       (funcall function)
;;                       (done pool t-name))
;;                     :name t-name)))
;;     (setf (gethash t-name (threads pool)) thread)
;;     thread))

(defmethod done ((pool thread-pool) thread-name)
  "Signal that a thread is done and remove it from the pool."
  (with-mutex (mutex pool)
    ;; (bt:destroy-thread (gethash thread-name (threads pool)))
    ;; join-thread
    ;; (bt:join-thread (gethash thread-name (threads pool)))
    (remhash thread-name (threads pool)))
  (signal-condition (conditionv pool)))

(defmethod done ((pool thread-pool) (worker task-worker))
  (with-mutex (mutex pool)
    (incf (worker-task-count worker))
    (setf (worker-state worker) :done)
    (signal-condition (conditionv (worker-pool worker)))
    (if (eq :finish (worker-on-finish worker))
      (stop-task-worker (worker-pool worker) worker)
      (when (or (eq :wait (worker-on-finish worker))
              (and (eq :query (worker-on-finish worker)) (query-next (worker-pool worker) worker)))
        (setf (worker-state worker) :waiting)
        ;; (enqueue (worker-queue (worker-pool worker)) worker)
        ))
    (signal-condition (conditionv pool))))

(defmethod resize-pool ((pool thread-pool) new-size)
  "Resize the thread pool to NEW-SIZE threads."
  (with-mutex (mutex pool)
    (when (< new-size (thread-count pool))
      (error "New size is less than current thread count"))
    (setf (thread-limit pool) new-size))) ;; set the limit

;; join-pool-threads
(defmethod join-pool-threads ((pool thread-pool))
  "Wait for all threads in the pool to finish."
  (with-mutex (mutex pool)
    (loop while (plusp (thread-count pool))
      do (wait-on-condition (conditionv pool) (mutex pool)))))

(defmethod stop-thread ((pool thread-pool) thread)
  "Stop a thread and remove it from the pool."
  (with-mutex (mutex pool)
    (bt:destroy-thread thread)
    (remhash thread (threads pool))))

(defmethod active-threads ((pool thread-pool))
  "Return a list of active threads."
  (maphash #'(lambda (key value)
               (declare (ignore value))
               key)
    (threads pool)))

(defmethod thread-count ((pool thread-pool))
  "Return the number of threads in the pool."
  (hash-table-count (threads pool)))

(defmethod in-pool ((pool thread-pool) thread)
  "Return T if THREAD is in the pool."
  (with-mutex (mutex pool)
    (gethash thread (threads pool))))

(defmethod thread-running-p ((pool thread-pool) thread)
  "Return T if THREAD is running."
  (with-mutex (mutex pool)
    (bt:thread-alive-p thread)))
(defmethod spawn-task ((pool thread-pool) (task t))
  "Spawn a task in the thread pool."
  (spawn-task pool
    (new-task (lambda () (funcall task)))))
(defmethod spawn-task ((pool thread-pool) (task async-task))
  "Spawn a task in the thread pool."
  (with-mutex (mutex pool)
    (clevelib.queue:enqueue (task-queue pool) task)
    (signal-condition (conditionv pool))))

;; task-workers will not teardown on their own but
;; will wait for new tasks to be added to the queue:


(define-condition task-worker-error (error)
  ((task-worker :initarg :task-worker :reader task-worker)
    (task :initarg :task :reader task)
    (description :initarg :description :reader description)
    (state :initarg :state :reader state)))



(defmethod print-object ((worker task-worker) stream)
  (print-unreadable-object (worker stream :type t :identity t)
    (format stream "~a[State:~A;Jobs:~A]" (worker-description worker) (worker-state worker) (worker-task-count worker))))

(defmethod query-next ((pool thread-pool) (worker task-worker))
  "Query if the worker should continue running or exit gracefully."
  nil)

(defmethod finish-task ((worker task-worker))
  (with-mutex (mutex (worker-pool worker))
    (setf (worker-state worker) :done)
    (signal-condition (conditionv (worker-pool worker)))
    (if (eq :finish (worker-on-finish worker))
      (stop-task-worker (worker-pool worker) worker)
      (when (or (eq :wait (worker-on-finish worker))
              (and (eq :query (worker-on-finish worker)) (query-next (worker-pool worker) worker)))
        (setf (worker-state worker) :waiting)
        ;; (enqueue (worker-queue (worker-pool worker)) worker)
        ))))

(defmethod spawn-task-worker ((pool thread-pool) &key (description nil))
  "Make a task worker in the thread pool. This thread will wait for new tasks to
be added to the queue and not terminate on completion but dequeue the next task.
This is useful for long running tasks that need to be executed in the thread pool."
  (let* ((description (if description description (prin1-to-string (gensym "TaskWorker"))))
          (t-name (format nil "QueueWorker[P~d|~d]<~a>" (thread-count pool) (thread-limit pool)
                    description))
          (worker (make-task-worker :thread nil :task nil :pool pool :description description :state :waiting :err-state nil :err-message "" :on-finish :wait))
          (thread (bt:make-thread
                    (lambda ()
                      (loop while (plusp (thread-count pool))
                        do (let
                             ((task (clevelib.queue:dequeue (task-queue pool) t)))
                             (when task
                               (setf (worker-task worker) task)
                               (setf (worker-state worker) :running))
                             (sync-run task)
                             (done pool worker))))
                    :name t-name)))
    (setf (worker-thread worker) thread)
    (setf (gethash t-name (threads pool)) thread)
    worker))

(defmethod stop-task-worker ((pool thread-pool) (thread task-worker))
  "Stop a task worker and remove it from the pool."
  (stop-task-worker pool (worker-thread thread))
  ;; (remhash thread (threads pool))
  )

(defmethod stop-task-worker ((pool thread-pool) thread)
  "Stop a task worker and remove it from the pool."
                                        ;(with-mutex (mutex pool)
  (when (bt:thread-alive-p thread)
    (bt:destroy-thread thread))
  (loop for thread-v being the hash-keys of (threads pool)
    when (eq (gethash thread-v (threads pool)) thread)
    do (remhash thread-v (threads pool)))
  );)

(defmethod stop-all-task-workers ((pool thread-pool))
  "Stop all task workers and remove them from the pool."
  (with-mutex (mutex pool)
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (when (bt:thread-alive-p value)
                   (bt:destroy-thread value)))
      (threads pool))
    (clrhash (threads pool))))

(defmethod worker-p ((pool thread-pool) thread)
  "Return T if THREAD is a worker in the pool."
  (with-mutex (mutex pool)
    (and (typep thread 'task-worker)
      (eq pool (worker-pool thread)))))

(defmethod waiting-task-workers ((pool thread-pool))
  "Return a list of waiting task workers."
  (with-mutex (mutex pool)
    (loop for thread being the hash-values of (threads pool)
      when (and (bt:thread-alive-p thread)
             (not (clevelib.queue:queue-empty-p (task-queue pool))))
      collect thread)))

;; (defvar pool nil)
;; (defvar w nil)
;; (defvar w2 nil)
;; (setf pool (make-thread-pool :thread-limit 10))
;; (setf w (spawn-task-worker pool ))
;; (setf w2 (spawn-task-worker pool ))
;; (defvar tt nil )
;; (spawn-task pool (make-async-task
;;                    :function (lambda () (sleep 3) (log-info "hello ~a" (get-universal-time))  22)
;;                    :args nil
;;                    :status :pending
;;                    :promise (make-promise)
;;                    :lock (clevelib.thread:make-mutex)
;;                    :condition-variable (clevelib.thread:make-condition-variable)
;;                    :finished nil))
;; tt
;; (spawn-task pool tt)
;; w


;; ;;; Define the test suite
;; (defpackage :thread-pool-tests
;;   (:use :common-lisp :fiveam :bordeaux-threads :sb-thread :clevelib.queue))

;; (in-package :thread-pool-tests)

;; (def-suite thread-pool-suite
;;   :description "Unit tests for the thread pool and task worker functionality."
;;   :in thread-pool)
