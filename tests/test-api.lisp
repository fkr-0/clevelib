(uiop:define-package :clevelib.tests
  (:import-from
    :clevelib.api
    :message-system
    :translate-event-to-message
    #:events
    :emit
    :event-type
    :simple-sink
    :connected-sinks
    :on-message
    :on-event
    :simple-sink
    :simple-emitter)
  (:use :cl :fiveam))

(in-package :clevelib.tests)


(defclass some-event-class ()())
(defclass some-message-system-class ()())

;; Test suite
(def-suite clevelib-api-tests)

(defmethod event-type ((arg t))
  :test-event)
;; Test event emission and handling
(test event-emission-and-handling
  (let* ((test-sink (make-instance 'simple-sink))
          (test-event (make-instance 'some-event-class))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink))))
    (emit test-emitter test-event)
    (format t "Events: ~A~%" (events test-sink))
    (is (equal (first (events test-sink)) test-event))))

;; Test event to message translation
(test event-to-message-translation
  (let* ((test-sink (make-instance 'simple-sink :message-system (make-instance 'some-message-system-class)))
          (test-event (make-instance 'some-event-class))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink))))
    (emit test-emitter test-event)
    (is (not (null (translate-event-to-message (message-system test-sink) test-event))))))

;; Test message relaying
(test message-relaying
  (let* ((test-message-system (make-instance 'broadcasting-message-system)))
    (add-messenger test-message-system test-sink)
    (relay-message test-message-system test-message)
    (is (equal (first (events test-sink)) test-message))))

;; Test propagation mechanisms
(test propagation-mechanisms
  (let ((test-message (make-instance 'propagating-message :target-messengers (list test-sink))))
    (relay-message test-message-system test-message)
    (is (equal (first (events test-sink)) test-message))))

;; Test async execution and responses
(test async-execution-and-responses
  (let* ((test-message (make-instance 'async-message)))
    (add-async-exec-handler test-message-system test-message)
    (async-exec test-message #'some-task)
    (is (task-executed-p test-message))))

;; Define additional mock classes and methods for testing purposes
;; Define a mock event class
(defclass nested-event ()
  ((message-type :initform :nested :initarg :type :accessor message-type)))

;; Define mock message class
(defclass simple-message ()
  ((content :initform nil :initarg :content :accessor content)))

;; Override methods for testing purposes
(defmethod capture-p ((message simple-message) &rest args)
  (declare (ignorable args))
  nil)  ;; Default to nil for capture

(defmethod bubble-p ((message simple-message) &rest args)
  (declare (ignorable args))
  nil)  ;; Default to nil for bubble

;; Nested Events without Capture or Bubble
;; (test nested-events-no-capture-no-bubble
;;   (let ((*test-sink* (make-instance 'simple-sink))
;;          (*test-emitter* (make-instance 'simple-emitter :sinks (list *test-sink*)))
;;          (*nested-event* (make-instance 'nested-event)))
;;     (emit *test-emitter* *nested-event*)
;;     (is (equal (first (events *test-sink*)) *nested-event*))
;;     (is (not (capture-p *nested-event*)))
;;     (is (not (bubble-p *nested-event*)))
;;     ))

;; Nested Events with Capture Enabled
(defmethod capture-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable capture for nested events

(test nested-events-with-capture
  (let* ((test-sink (make-instance 'simple-sink))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink)))
          (nested-event (make-instance 'nested-event)))
    (emit test-emitter nested-event)
    (is (equal (first (events test-sink)) nested-event))
    (is (capture-p nested-event))
    ))

;; Nested Events with Bubble Enabled
(defmethod bubble-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable bubble for nested events

(test nested-events-with-bubble
  (let* ((test-sink (make-instance 'simple-sink))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink)))
          (nested-event (make-instance 'nested-event)))
    (emit test-emitter nested-event)
    (is (equal (first (events test-sink)) nested-event))
    (is (bubble-p nested-event))
    ))

;; Nested Events with Both Capture and Bubble Enabled
(test nested-events-with-capture-and-bubble
  (let* ((test-sink (make-instance 'simple-sink))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink)))
          (nested-event (make-instance 'nested-event)))
    (emit test-emitter nested-event)
    (is (equal (first (events test-sink)) nested-event))
    (is (capture-p nested-event))
    (is (bubble-p nested-event))
    ))

;; More Complex Scenarios

;; Nested Events
(test nested-events-handling
  (let* ((nested-event (make-instance 'nested-event-class))
          (test-sink (make-instance 'simple-sink))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink))))
    (emit test-emitter test-event)
    (emit test-emitter nested-event)
    (is (equal (second (events test-sink)) nested-event))))

;; Async Responses
(test async-responses-handling
  (let* ((test-message (make-instance 'async-message))
          (test-message-system (make-instance 'broadcasting-message-system)))
    (add-async-exec-handler test-message-system test-message)
    (async-exec test-message (lambda () (format t "Async task executed~%")))
    (is (task-executed-p test-message))
    (respond test-message (make-instance 'response-message))
    (is (response-received-p test-message))))

;; ;; Complex Event Chains
(test complex-event-chains
  (let* ((initial-event (make-instance 'initial-event))
          (trigger-event (make-instance 'trigger-event))
          (response-event (make-instance 'response-event))
          (test-sink (make-instance 'simple-sink))
          (test-emitter (make-instance 'simple-emitter :sinks (list test-sink))))
    (emit test-emitter initial-event)
    (is (equal (first (events test-sink)) initial-event))
    ;; Trigger the next event
    (emit test-emitter trigger-event)
    (is (equal (second (events test-sink)) trigger-event))
    ;; Respond to the triggered event
    (emit test-emitter response-event)
    (is (equal (third (events test-sink)) response-event))))



(defclass complex-sink (simple-sink)
  ((events :initform nil :initarg :events :accessor cs-events)
    (parent :initform nil :initarg :parent :accessor parent)
    (children :initform nil :initarg :children :accessor children)))
(defmethod events ((s t))
  (cs-events s))
;; (defmethod connected-sinks ((s simple-emitter) e &rest args)
;;   (declare (ignorable args))
;;   )

(defmethod get-parent ((sink complex-sink))
  (parent sink))

(defmethod get-children ((sink complex-sink))
  (children sink))

(defmethod on-event ((sink complex-sink) event &rest args)
  (declare (ignorable args))
  (push event (events sink))
  (on-message sink (translate-event-to-message sink event))
  (format t "Event received at ~A: ~A~%" sink event))

;; Helper function to create a hierarchy of sinks
(defun create-hierarchy ()
  (let* ((root (make-instance 'complex-sink))
          (child1 (make-instance 'complex-sink :parent root))
          (child2 (make-instance 'complex-sink :parent root))
          (grandchild1 (make-instance 'complex-sink :parent child1))
          (grandchild2 (make-instance 'complex-sink :parent child1)))
    (setf (children root) (list child1 child2))
    (setf (children child1) (list grandchild1 grandchild2))
    (list root child1 child2 grandchild1 grandchild2)))

;; Test complex hierarchies without capture or bubble
(test complex-hierarchy-no-capture-no-bubble
  (let* ((hierarchy (create-hierarchy))
          (root (first hierarchy))
          (event (make-instance 'nested-event)))
    (on-event root event)
    (dolist (sink hierarchy)
      (is (not (capture-p event)))
      (is (not (bubble-p event)))
      (is (member event (events sink) :test 'equal)))))

;; Test complex hierarchies with capture enabled at different levels
(defmethod capture-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable capture for nested events

(test complex-hierarchy-with-capture
  (let* ((hierarchy (create-hierarchy))
          (root (first hierarchy))
          (event (make-instance 'nested-event)))
    (emit root event)
    (dolist (sink hierarchy)
      (is (capture-p event))
      (is (member event (events sink) :test 'equal)))))

;; Test complex hierarchies with bubble enabled at different levels
(defmethod bubble-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable bubble for nested events

(test complex-hierarchy-with-bubble
  (let* ((hierarchy (create-hierarchy))
          (root (first hierarchy))
          (event (make-instance 'nested-event)))
    (emit root event)
    (dolist (sink hierarchy)
      (is (bubble-p event))
      (is (member event (events sink) :test 'equal)))))

;; Test complex hierarchies with both capture and bubble enabled
(defmethod capture-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable capture for nested events

(defmethod bubble-p ((message nested-event) &rest args)
  (declare (ignorable args))
  t)  ;; Enable bubble for nested events

(test complex-hierarchy-with-capture-and-bubble
  (let* ((hierarchy (create-hierarchy))
          (root (first hierarchy))
          (event (make-instance 'nested-event)))
    (emit root event)
    (dolist (sink hierarchy)
      (is (capture-p event))
      (is (bubble-p event))
      (is (member event (events sink) :test 'equal)))))
