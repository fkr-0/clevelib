
(in-package :clevelib-tests)
(def-suite threads-test-suite
  :description "Test suite for the clevelib.threads package"
  :in testmain
  )

(format t "~&~%Running tests for clevelib.threads package~%~%")
(in-suite threads-test-suite)

(test create-destroy-thread-thread-tests
  "Test creating and destroying a thread."
  (let* (
          (test-thread (clevelib.threads:create-thread (lambda () (sleep 0.5)))))
    (is (typep test-thread 'bt:thread))
    ;; (is (clevelib.threads:get-thread (bt:thread-name test-thread)) )
    (unwind-protect
      (clevelib.threads:destroy test-thread )
      (sleep 0.5)) ;; Give thread a chance to finish
    ;; (clevelib.threads:destroy test-thread )
    ;; (is (null (clevelib.threads:get-thread (bt:thread-name test-thread) )))
    ))

(test mutex-locking-thread-tests
  "Test mutex locking and unlocking."
  (let ((test-mutex (clevelib.threads:make-mutex)))
    (is (typep test-mutex 'bt:lock))
    (clevelib.threads:with-mutex test-mutex
      (is (sb-thread:holding-mutex-p test-mutex)))))

(test condition-variable-thread-tests
  "Test creating and signaling a condition variable."
  (let ((threads (make-hash-table :test 'equal))
         (test-mutex (clevelib.threads:make-mutex))
         (test-condition (clevelib.threads:make-condition-variable)))
    (is (typep test-condition 'sb-thread:waitqueue))
    (clevelib.threads:create-thread (lambda () (with-mutex test-mutex (clevelib.threads:wait-on-condition test-condition test-mutex 3))) )
    ;; (clevelib.threads:create-thread (lambda () ()) threads)
    (clevelib.threads:signal-condition test-condition)
    (sleep 0.3) ;; Give other thread a chance to wait on the condition
    (clevelib.threads:with-mutex test-mutex
      (clevelib.threads:signal-condition test-condition))
    (clevelib.threads:signal-condition test-condition)
    ;; No check here: if thread does not wake up, the test will hang
    ))
(test cleanup-thread-tests
  "Test cleanup function."
  (let* ((test-thread (clevelib.threads:create-thread (lambda () (loop (sleep 0.5)))))
          (test-thread-name (bt:thread-name test-thread)))
    (is (typep test-thread 'bt:thread))
    ;; (is (clevelib.threads:get-thread (bt:thread-name test-thread)))
    ;; (clevelib.threads:cleanup)
    (clevelib.threads:destroy test-thread)
    ;; (is (null  (clevelib.threads:get-thread test-thread-name) ))
    ))
