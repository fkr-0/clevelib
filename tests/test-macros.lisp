(in-package :clevelib-tests)

(def-suite macros-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain
  )

(format t "~&~%Running tests for clevelib.macros package~%~%")
(in-suite macros-test-suite)

;; Test the on macro
(test on-macro
  (clear-handlers)

  (let ((event-target (make-instance 'dummy-target))
         (event-triggered nil))
    (on :test-event event-target (setf event-triggered t))
    (trigger :test-event event-target)
    (is (not (null event-triggered)))))

;; Test the trigger macro
(test trigger-macro
  (let ((event-target 'dummy-target)
         (event-triggered nil))
    (on :test-event event-target (setf event-triggered t))
    (trigger :test-event event-target)
    (is (not (null event-triggered)))))

;; Test the defevent macro
;; (test defevent-macro
;;   (let ((event-target (make-instance 'dummy-target))
;;          (event-triggered nil))
;;     (defevent test-event-handler :test-event (make-instance 'dummy-target)
;;       (setf event-triggered t))
;;     (trigger :test-event event-target)
;;     (is (not (null event-triggered)))))


;; Test the bind function
(test bind-function
  (clear-handlers)
  (let ((event-target (make-instance 'dummy-target))
         (event-triggered nil))
    ;; (funcall
    (bind event-target :test-event
      (lambda (x) (setf event-triggered t)))
    ;; )
    (trigger :test-event event-target)
    (is (not (null event-triggered)))))
;; (run-all-tests)

(test test-bind-with-callback
  (clear-handlers)
  (let ((target (make-instance 'dummy-target))
         (event :click)
         (callback (lambda (event) (print "Clicked!"))))
    (bind target event callback)
    (is (equal (length (gethash event *handlers*)) 1))))

(test test-bind-without-callback
  (clear-handlers)
  ;; (format t "Handlers: ~a~%" *handlers*)
  (let ((target (make-instance 'dummy-target))
         (event :click))
    (let ((closure (bind target event)))

      ;; (format t "Handlers: ~a~%" *handlers*)
      (is (equal (gethash event *handlers*) nil))
      (funcall closure (lambda (event) (print "Clicked!")))
      ;; (format t "Handlers: ~a~%" *handlers*)
      (is (equal (length (gethash event *handlers*)) 1)))))

(test test-bind-multiple-callbacks
  (clear-handlers)
  (let ((target (make-instance 'dummy-target))
         (event :click)
         (callback1 (lambda (event) (print "Clicked 1!")))
         (callback2 (lambda (event) (print "Clicked 2!"))))

    (bind target event callback1)
    (bind target event callback2)

    (is (equal (length (gethash event *handlers*)) 2))))

(test test-trigger
  (let ((target (make-instance 'dummy-target)))
    (bind target :click (lambda (event) (setf *test-output* "Clicked!")))
    (trigger :click target)
    (is (equal *test-output* "Clicked!"))))

(test test-on
  (let ((target (make-instance 'dummy-target)))
    (on :click target (setf *test-output* "Clicked!"))
    (trigger :click target)
    (is (equal *test-output* "Clicked!"))))

(test test-deflistener
  (let ((target (make-instance 'dummy-target)))
    (deflistener handle-click :click target
      (setf *test-output* "Clicked!"))
    (trigger :click target)
    (is (equal *test-output* "Clicked!"))))

(test test-with-event-loop
  (let ((target (make-instance 'dummy-target)))
    (with-event-loop (:async t :until nil)
      (on :click target (setf *test-output* "Clicked!"))
      (trigger :click target))
    (is (equal *test-output* "Clicked!"))))

;; (test test-live-update
;;   (let ((counter 0))
;;     (live-update (incf counter) :interval 0.1)
;;     (sleep 0.5)
;;     (is (>= counter 4))))

(test test-defasync
  (let ((result nil))
    (funcall (defasync test-async
               (setq result 42)))
    (sleep 1) ; Wait for async execution to complete
    (is (equal result 42))))
