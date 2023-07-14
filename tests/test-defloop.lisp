(in-package :clevelib-tests)

(def-suite defloop-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain
  )

(in-suite defloop-test-suite)

(test defloop-test-1
  (let ((eve (make-instance 'clevelib.event-system:event-system))
         (x "dud"))
    (defloop no-1 evevevent ()
      (format nil "no2 ~a" evevevent))
    (is (equal x "dud") "x should be 0")
    (loop for v in (list #'no-1-out-queue #'no-1-in-queue #'no-1-state #'no-1-run #'no-1-stop #'no-1-loop)
      do (is (functionp v)))
    (no-1-run eve)
    (clevelib.queue:enqueue (no-1-in-queue eve) "luululu")

    (is (string-equal (clevelib.queue:dequeue-wait (no-1-out-queue eve)) "no2 luululu"))
    ))

;; (def-test defloop-test-2
;; TODO connecting different defloops from in/outloop
