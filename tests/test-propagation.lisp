;; File: propagation-test.lisp
;;
;; Purpose: This file provides unit tests for the event propagation functions in propagation.lisp.
;;

(defpackage :event-propagation-test
  (:use :cl :rt :event-propagation))

(in-package :event-propagation-test)

;; Unit tests for trigger-event function
(deftest trigger-event-test
  (let ((target (make-instance 'event-target :id "test-target"))
        (event-type :test-event)
        (args '(1 2 3)))
    (spyx (trigger-event event-type target args))
    (is (equal (event-target-id (first (event-propagation-targets (first *event-propagation-list*))))
               "test-target"))
    (is (equal (event-type (first (event-propagation-events (first *event-propagation-list*))))
               :test-event))
    (is (equal (first (event-propagation-args (first *event-propagation-list*)))
               args)))))
