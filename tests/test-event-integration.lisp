(def-test test-event-system-integration-1
  "Integration test for the complete async event loop and event-system facilities."
  (let ((event-target-1 (create-dummy-target))
         (event-target-2 (create-dummy-target))
         (event-callback-1 (lambda (event) (push 'event-1 (event-data event))))
         (event-callback-2 (lambda (event) (push 'event-2 (event-data event))))
         (event-data '()))
    (add-event-listener 'integration-event-1 event-target-1 event-callback-1)
    (add-event-listener 'integration-event-2 event-target-2 event-callback-2)
    (async-exec
      (lambda ()
        (sleep 0.5)
        (trigger-event 'integration-event-1 event-target-1 :data event-data)))
    (async-exec
      (lambda ()
        (sleep 1.0)
        (trigger-event 'integration-event-2 event-target-2 :data event-data)))
    (process-events 2)
    (let ((event-list-1 (event-target-triggered-events event-target-1))
           (event-list-2 (event-target-triggered-events event-target-2)))
      (is (= 1 (length event-list-1)))
      (is (= 1 (length event-list-2)))
      (is (eq (event-type (car event-list-1)) 'integration-event-1))
      (is (eq (event-type (car event-list-2)) 'integration-event-2))
      (is (equal event-data '(event-2 event-1))))))
(def-test test-event-system-integration-2
  "Integration test for the complete async event loop and event-system facilities with event capturing/bubbling."
  (let ((event-target-parent (create-dummy-target))
         (event-target-child (create-dummy-target))
         (event-callback-parent (lambda (event) (push 'parent (event-data event))))
         (event-callback-child (lambda (event) (push 'child (event-data event))))
         (event-data '()))
    (setf (event-target-parent event-target-child) event-target-parent)
    (add-event-listener 'integration-event event-target-parent event-callback-parent :capture t)
    (add-event-listener 'integration-event event-target-child event-callback-child)
    (async-exec
      (lambda ()
        (sleep 0.5)
        (trigger-event 'integration-event event-target-child :data event-data)))
    (process-events 1)
    (let ((event-list-parent (event-target-triggered-events event-target-parent))
           (event-list-child (event-target-triggered-events event-target-child)))
      (is (= 1 (length event-list-parent)))
      (is (= 1 (length event-list-child)))
      (is (eq (event-type (car event-list-parent)) 'integration-event))
      (is (eq (event-type (car event-list-child)) 'integration-event))
      (is (equal event-data '(child parent)))))
