(in-package :clevelib-tests)
(def-suite queue-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain

  )

(in-suite queue-test-suite)
(use-package :clevelib.queue)

(test enqueue-normal-priority
  "Test enqueue method with normal priority."
  (let ((queue (make-queue)))
    (enqueue queue 'item)
    (is (= (queue-length queue) 1))
    (is (eq (dequeue-no-wait queue) 'item))))
(let ((q (clevelib.queue:make-queue)))
  (clevelib.queue:enqueue q 1)
  (clevelib.queue:enqueue q 2)
  (clevelib.queue:dequeue-no-wait q)q)
(test enqueue-high-priority
  "Test enqueue method with high priority."
  (let ((queue (make-queue)))
    (enqueue queue 'item1 :normal)
    (enqueue queue 'item2 :high)
    (is (= (queue-length queue) 2))
    (is (eq (dequeue-no-wait queue) 'item2))
    (is (eq (dequeue-no-wait queue) 'item1))))

(test dequeue-normal-priority
  "Test dequeue method with normal priority."
  (let ((queue (make-queue)))
    (enqueue queue 'item)
    (is (eq (dequeue queue) 'item))))

(test dequeue-high-priority
  "Test dequeue method with high priority."
  (let ((queue (make-queue)))
    (enqueue queue 'item1 :normal)
    (enqueue queue 'item2 :high)
    (is (eq (dequeue queue) 'item2))
    (is (eq (dequeue queue) 'item1))))

(test dequeue-no-wait-empty-queue
  "Test dequeue-no-wait on an empty queue. It should return nil."
  (let ((queue (make-queue)))
    (is (null (dequeue-no-wait queue)))))

(test dequeue-no-wait-nonempty-queue
  "Test dequeue-no-wait on a nonempty queue. It should return the highest priority item."
  (let ((queue (make-queue)))
    (enqueue queue 'item)
    (is (eq (dequeue-no-wait queue) 'item))
    (is (null (dequeue-no-wait queue)))))

(test queue-length-empty-queue
  "Test queue-length on an empty queue. It should return 0."
  (let ((queue (make-queue)))
    (is (zerop (queue-length queue)))))

(test queue-length-nonempty-queue
  "Test queue-length on a nonempty queue. It should return the number of items in the queue."
  (let ((queue (make-queue)))
    (enqueue queue 'item1 :normal)
    (enqueue queue 'item2 :high)
    (is (= (queue-length queue) 2))
    (dequeue-no-wait queue)
    (is (= (queue-length queue) 1))
    (dequeue-no-wait queue)
    (is (zerop (queue-length queue)))))

(test multi-threaded-queue-test
  "Multi-threaded integration test for the priority queue"
  (let ((queue (make-queue))
         (results (make-array 200 :initial-element nil))
         (high-priority-items (loop for i from 1 to 100 collect (cons :high i)))
         (low-priority-items (loop for i from 101 to 200 collect (cons :normal i))))

    ;; Enqueue all items
    (loop for item in (nconc high-priority-items low-priority-items)
      do (enqueue queue (cdr item) (car item)))

    ;; Worker 1, dequeues without waiting
    (let ((worker-1 (bt:make-thread
                      (lambda ()
                        (loop for i from 0 below 100
                          do (setf (aref results i) (dequeue-no-wait queue)))))))

      ;; Worker 2, dequeues with waiting
      (let ((worker-2 (bt:make-thread
                        (lambda ()
                          (loop for i from 100 below 200
                            do (setf (aref results i) (dequeue queue)))))))

        ;; Wait for both workers to finish
        (bt:join-thread worker-1)
        (bt:join-thread worker-2)
        (is (eq (dequeue-no-wait queue) nil))


        ;; Check results
        ;; (loop for i from 1 to 200
        ;;   do (is (= i (aref results (- i 1)))))
        ))))



(test multi-threaded-queue-test-2
  "Another multi-threaded integration test for the priority queue"
  (let ((queue (make-queue))
         (one-fin nil)
         (two-fin nil)
         (test-item (cons :high 1)))

    ;; Worker 1, dequeue with waiting
    (let ((worker-1 (bt:make-thread
                      (lambda ()
                        (let ((result (dequeue queue)))
                          (setf one-fin result))))))

      ;; Worker 2, dequeue without waiting
      ;; (let ((worker-2 ))
      (bt:make-thread
        (lambda ()
          (let ((result (dequeue-no-wait queue)))
            (setf two-fin result))))
      ;; Sleep a while
      (sleep 0.1)

      ;; Check unfinished condition for worker 1
      (is (not one-fin))

      ;; Check finished condition for worker 2
      (is (eq two-fin nil))
      ;; Enqueue an item
      (enqueue queue (cdr test-item) (car test-item))
      ;; Wait for worker 1 to finish
      (bt:join-thread worker-1)

      ;; Check worker 1 is finished
      (is (= one-fin 1))

      ;; Check both workers are finished
      (is (eq two-fin nil)))))


;; Test that an event can be enqueued and dequeued
;; (test test-enqueue-dequeue-event
;;   (let ((event-queue (make-event-queue)))
;;     (enqueue event-queue "test-event")
;;     (is (equal (dequeue event-queue) "test-event"))))

;; ;; Test that the priority of an event can be changed
;; (test test-change-event-priority
;;   (let ((equeue (make-priority-queue)))
;;     (enqueue-prio equeue "test-event")
;;     (change-event-priority equeue :high)
;;     (is (equal (priority equeue) :high))))

;; ;; Test that an event queue can be checked for emptiness
;; (test test-event-queue-empty-p
;;   (let ((event-queue (make-event-queue)))
;;     (is (event-queue-empty-p event-queue))
;;     (enqueue event-queue "test-event")
;;     (is (not (event-queue-empty-p event-queue)))))


;; ;; Test that the length of an event queue can be determined
;; (test test-event-queue-length
;;   (let ((event-queue (make-event-queue)))
;;     (is (= (event-queue-length event-queue) 0))
;;     (enqueue event-queue "test-event")
;;     (is (= (event-queue-length event-queue) 1))))


(test typed-queue
  (let ((q (make-instance 'typed-queue :type 'number))
         (q1 (make-instance 'typed-queue :type 'string))
         (q2 (make-instance 'typed-queue :type 'symbol)))

    (enqueue q 1)
    (enqueue q 2)
    (enqueue q2 :dud)
    (enqueue q1 "dud")
    (enqueue q 3)
    (is (equal (dequeue q) 1))
    (is (equal (dequeue q) 2))
    (is (equal (dequeue q1) "dud"))
    (is (equal (dequeue q2) :dud))
    (is (equal (dequeue q) 3))
    (handler-case
      (is (null (dequeue-no-wait q)))
      (wrong-type-condition (c)
        (is (eql 'number (expected-type c)))
        (is (eql 'null (actual-type c)))))))


(test typed-queue-w-e
  (let ((q (make-instance 'typed-queue :type 'integer)))
    (enqueue q 1)
    (enqueue q 2)
    (enqueue q 3)
    (is (eql (dequeue-no-wait q) 1))
    (is (eql (dequeue-wait q) 2))
    (is (eql (dequeue-no-wait q) 3))))

(test wrong-type-error-1
  (let ((queue (make-instance 'typed-queue :type 'number)))
    (handler-case
      (progn
        (enqueue queue "not a number")
        (is (eql "not a number" (dequeue queue))))
      (wrong-type-condition (c)
        (is (eql 'number (expected-type c)))
        (is (equalp (type-of "not a number")
              (actual-type c)))))))

(test wrong-type-error-2
  (let ((queue (make-instance 'typed-queue :type 'string)))
    (handler-case
      (progn
        (enqueue queue 33)
        (is nil))
      (wrong-type-condition (c)
        (is (eql (type-of 33) (actual-type c)))
        (is (eql 'string (expected-type c)))))))
