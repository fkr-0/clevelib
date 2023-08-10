(in-package :clevelib.threads)

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
    ;; (thread-count :initarg :thread-count
    ;;   :accessor thread-count
    ;;   :initform 0 :documentation "The number of threads in the pool.")
    (thread-limit :initarg :thread-limit :accessor thread-limit :initform 0
      :documentation "The maximum number of threads allowed in the pool. 0 means no limit."))
  (:documentation "A thread pool that manages thread creation, synchronization, and cleanup."))

(defmethod initialize-instance :after ((pool thread-pool) &key)
  "Initialize a thread pool."
  ;; (declare (ignorable key))
  (setf (threads pool) (make-hash-table)
    (mutex pool) (make-mutex)
    (conditionv pool) (make-condition-variable)))



;;; Primitives
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



;;; API
;;;
;;; The following functions are the public API for the thread pool.
;;; They are wrappers around the primitives above.
;;;
;;; The thread pool is a singleton, so there is no need to pass it
;;; around. The thread pool is created when the library is loaded.
;;; The thread pool is destroyed when the library is unloaded.
;;;
(defun make-thread-pool (&key (thread-limit 0))
  "Create a new thread pool with THREAD-LIMIT threads."
  (make-instance 'thread-pool :thread-limit thread-limit))

(defmethod start-thread ((pool thread-pool) function &key (description t))
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

(defmethod done ((pool thread-pool) thread-name)
  "Signal that a thread is done and remove it from the pool."
  (with-mutex (mutex pool)
    (remhash thread-name (threads pool))
    (signal-condition (conditionv pool))))

(defmethod resize ((pool thread-pool) new-size)
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
               key) (threads pool)))

(defmethod thread-count ((pool thread-pool))
  "Return the number of threads in the pool."
  (hash-table-count (threads pool)))
