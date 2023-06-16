(in-package :clevelib-tests)
(def-suite queue-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain
  )

(in-suite queue-test-suite)

;; Test that an event can be enqueued and dequeued
(test test-enqueue-dequeue-event
  (let ((event-queue (make-event-queue)))
    (enqueue event-queue "test-event")
    (is (equal (dequeue event-queue) "test-event"))))

;; Test that the priority of an event can be changed
(test test-change-event-priority
  (let ((equeue (make-priority-queue)))
    (enqueue-prio equeue "test-event")
    (change-event-priority equeue :high)
    (is (equal (priority equeue) :high))))

;; Test that an event queue can be checked for emptiness
(test test-event-queue-empty-p
  (let ((event-queue (make-event-queue)))
    (is (event-queue-empty-p event-queue))
    (enqueue event-queue "test-event")
    (is (not (event-queue-empty-p event-queue)))))


;; Test that the length of an event queue can be determined
(test test-event-queue-length
  (let ((event-queue (make-event-queue)))
    (is (= (event-queue-length event-queue) 0))
    (enqueue event-queue "test-event")
    (is (= (event-queue-length event-queue) 1))))
