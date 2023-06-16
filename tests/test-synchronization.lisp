(in-package :clevelib-tests)

(def-suite synchronization-tests
  :in testmain
  :before-all (format t "Running synchronization tests.~%")
  :after-all (format t "Finished synchronization tests.~%"))

(deftest with-event-mutex-test
  (let ((mutex (make-event-mutex)))
    (with-event-mutex mutex
      (assert (event-mutex-p mutex)))))

(deftest event-condition-variable-wait-test
  (let ((mutex (make-event-mutex))
         (condition-variable (make-event-condition-variable)))
    (let ((thread (make-thread (lambda ()
                                 (sleep 1)
                                 (with-event-mutex mutex
                                   (condition-variable-notify (event-condition-variable-condition-variable condition-variable))))))
           (start-time (get-internal-real-time)))
      (event-condition-variable-wait condition-variable mutex 2)
      (let ((end-time (get-internal-real-time)))
        (assert (<= (- end-time start-time) 2000000))))))

(in-package :event-system.synchronization-test)

(def-suite synchronization-test-suite
  :in-order-to ((test . run-synchronously))
  :defaults (synchronization-test-case :reporter (make-instance 'fiveam-text-ui)))

(def-test-group event-mutex-test-group (synchronization-test-suite)
  (let ((mutex (make-event-mutex)))
    (test "with-event-mutex acquires and releases the mutex"
      (with-event-mutex mutex
        (assert (bordeaux-threads:mutex-locked-p (event-mutex-mutex mutex))))
      (assert (not (bordeaux-threads:mutex-locked-p (event-mutex-mutex mutex)))))))

(def-test-group event-condition-variable-test-group (synchronization-test-suite)
  (let ((mutex (make-event-mutex))
         (condition-variable (make-event-condition-variable)))
    (test "event-condition-variable-wait waits for a signal"
      (bordeaux-threads:make-thread
        (lambda ()
          (bordeaux-threads:with-lock-held ((event-mutex-mutex mutex))
            (sleep 1)
            (event-condition-variable-notify condition-variable))))
      (with-event-mutex mutex
        (let ((start-time (get-internal-real-time)))
          (event-condition-variable-wait condition-variable mutex 2)
          (assert (< (- (get-internal-real-time) start-time) 2000)))))))

;;;; END: 8f7e3a1b5c2d
