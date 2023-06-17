;; FILEPATH: /home/user/code/lisp/clli/test/async/event-loops-test.lisp
;; BEGIN: 7f8e9d2b0f3d
(in-package :clevelib-tests)
(def-suite event-loops-test
  :in testmain)

(in-suite event-loops-test)

(defmacro with-event-loop ((id) &body body)
  `(let ((event-loop (make-event-loop ,id)))
     (unwind-protect
       (progn
         (start-event-loop ,id)
         ,@body)
       (stop-event-loop ,id))))

(test test-enqueue-event
  (with-event-loop ("test-enqueue-event")
    (let ((event (make-instance 'event :target "target" :data "data")))
      (enqueue-event event)
      (sleep 0.1)
      (is (equal (event-target event) "target"))
      (is (equal (event-data event) "data")))))

(test test-trigger-error-event
  (with-event-loop ("test-trigger-error-event")
    (let ((event (make-instance 'event :target "target" :data "data")))
      (trigger-error-event event "error-target")
      (sleep 0.1)
      (is (equal (event-target event) "error-target"))
      (is (equal (event-data event) "data")))))

(test test-add-event-listener
  (with-event-loop ("test-add-event-listener")
    (let ((event (make-instance 'event :target "target" :data "data")))
      (add-event-listener :test t (lambda (event target)
                                    (is (equal target "target"))
                                    (is (equal (event-data event) "data"))))
      (enqueue-event event)
      (sleep 0.1))))

(test test-remove-event-listener
  (with-event-loop ("test-remove-event-listener")
    (let ((event (make-instance 'event :target "target" :data "data"))
           (listener (lambda (event target)
                       (is (equal target "target"))
                       (is (equal (event-data event) "data")))))
      (add-event-listener :test t listener)
      (remove-event-listener :test t listener)
      (enqueue-event event)
      (sleep 0.1))))

(test test-toggle-event-loop
  (with-event-loop ("test-toggle-event-loop")
    (let ((event (make-instance 'event :target "target" :data "data")))
      (toggle-event-loop "test-toggle-event-loop")
      (enqueue-event event)
      (sleep 0.1)
      (toggle-event-loop "test-toggle-event-loop"))))

;; END: 7f8e9d2b0f3d;; FILEPATH: /home/user/code/lisp/clli/test/async/event-loops-test.lisp
;; BEGIN: xz8y3n4k9d1p
(test test-enqueue-event
  (let ((event (list :event-type 'test-event :data "test-data")))
    (enqueue-event event)
    (is (equal (dequeue *event-queue*) event))))

(test test-make-event-loop
  (let ((id :test-loop))
    (make-event-loop id)
    (is (typep (get-event-loop id) 'priority-event-queue))))

(test test-get-event-loop
  (let ((id :test-loop))
    (make-event-loop id)
    (is (typep (get-event-loop id) 'priority-event-queue))))

(test test-event-loop-active-p
  (let ((id :test-loop))
    (start-event-loop id)
    (is (event-loop-active-p id))
    (stop-event-loop id)
    (is (not (event-loop-active-p id)))))

(test test-trigger-error-event
  (let ((event (list :event-type 'error :data "error-data"))
         (target :test-target))
    (trigger-error-event event target)
    (is (equal (dequeue *event-queue*) (cons event target)))))

(test test-process-loop-events
  (let ((event (list :event-type 'test-event :data "test-data")))
    (enqueue-event event)
    (process-loop-events)
    (is (null (dequeue *event-queue*)))))

(test test-start-event-loop
  (let ((id :test-loop))
    (start-event-loop id)
    (is (event-loop-active-p id))
    (stop-event-loop id)
    (is (not (event-loop-active-p id)))))

(test test-stop-event-loop
  (let ((id :test-loop))
    (start-event-loop id)
    (stop-event-loop id)
    (is (not (event-loop-active-p id)))))

(test test-toggle-event-loop
  (let ((id :test-loop))
    (toggle-event-loop id)
    (is (event-loop-active-p id))
    (toggle-event-loop id)
    (is (not (event-loop-active-p id)))))
;; END: xz8y3n4k9d1p;; FILEPATH: /home/user/code/lisp/clli/tests/event-loops-test.lisp
;; BEGIN: 1a2b3c4d5e6f
(in-package :clevelib.event-loops.test)

(defmacro with-event-loop ((id) &body body)
  `(let ((event-loop (make-event-loop ,id)))
     (unwind-protect
       (progn ,@body)
       (stop-event-loop ,id))))

(test test-enqueue-event
  (with-event-loop ("test-enqueue-event")
    (enqueue-event '(test-event . "test-data"))
    (let ((event (bt:with-lock-held (*event-lock*)
                   (dequeue *event-queue*))))
      (is (equal event '(test-event . "test-data"))))))


(test test-trigger-event
  (with-event-loop ("test-trigger-event")
    (let ((event (make-instance 'event :target "test-target" :data "test-data")))
      (enqueue-event event)
      (trigger-event event "test-target" "test-data")
      (is (equal (event-target event) "test-target"))
      (is (equal (event-data event) "test-data")))))

(test test-error-event-handler
  (with-event-loop ("test-error-event-handler")
    (let ((event (make-instance 'event :target "test-target" :data "test-data")))
      (add-event-listener :error t #'error-event-handler)
      (enqueue-event event)
      (trigger-error-event event "test-target")
      (is (equal (event-target event) "test-target"))
      (is (equal (event-data event) "test-data")))))
;; END: 1a2b3c4d5e6f(test test-enqueue-event
;; (let ((event (list :type :test-event :data "test data")))
;;   (enqueue-event event)
;;   (is (equal (dequeue-event *event-queue*) event))))

(test test-enqueue-event
  (let ((event (list :type :test-event :data "test data")))
    (enqueue-event event)
    (is (equal (dequeue-event *event-queue*) event))))

