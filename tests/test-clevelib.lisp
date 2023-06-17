(in-package :clevelib-tests)
;; Define your project tests here...
(def-suite testmain
  :description "Test suite for clevelib.macros package")

(in-suite testmain)

(defclass dummy-target ()
  ((test-event :initform nil :accessor test-event :initarg :test-event))
  (:documentation "Dummy target class for testing purposes"))


(test test-with-mutex
  (let ((mutex (clevelib.threads:make-mutex))
         (result nil))
    (with-mutex  mutex
      (setq result 1))
    (with-mutex mutex
      (setq result (+ result 1)))
    (is (equal result 2))))

(test test-with-condition-variable

  ;; (bt:make-thread
  ;;   (lambda ()
  ;; (format t "Thread started~%")
  (let ((condition-variable (clevelib.threads:make-condition-variable))
         (lock (clevelib.threads:make-mutex))
         (result nil))
    (bt:with-recursive-lock-held (lock)
      (bt:make-thread
        (lambda ()
          ;; (format t "Thread 2 started~%")
          (sleep 0.2)
          ;; (format t "Thread 2 notifying~%")
          (bt:with-recursive-lock-held (lock)

            (clevelib.threads:signal-condition condition-variable))))
      ;; (format t "Thread 1 waiting~%")
      (bt:with-recursive-lock-held (lock)

        (bt:condition-wait condition-variable lock)
        ;; (format t "Thread 1 notified~%")
        (setq result 42)))
    (is (equal result 42))));))
