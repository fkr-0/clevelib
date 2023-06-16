
;; File: clevelib/propagation.lisp
;;
;; Purpose: This file provides an implementation of event propagation, including
;;          capture, bubbling, and event delegation. It includes functions for
;;          triggering events, handling event propagation, and managing event
;;          delegation.
;;

(defpackage :event-propagation
  (:use :cl))

(in-package :event-propagation)

;; Functions for triggering events, handling event propagation, and managing event delegation
  ;; Trigger an event on a given target with optional arguments
(defun trigger-event (event target &rest args)
  "Trigger an event on a given target with optional arguments"
  (let ((event (make-event :type event
                           :target target
                           :args args)))
    (propagate-event event target)
    (handle-event-propagation event target args)))


(defun handle-event-propagation (event target &rest args)
  ;; Handle event propagation, including capture and bubbling phases
  )
(defun add-event-delegation (event target condition callback)
  "Add event delegation for a specific event, target, condition, and callback"
  (let ((delegation (make-event-delegation :event event
                                           :target target
                                           :condition condition
                                           :callback callback)))
    (setf (gethash event *event-delegations*)
          (acons target delegation (gethash event *event-delegations*)))
    (setf (gethash target *target-delegations*)
          (acons event delegation (gethash target *target-delegations*)))))

(defun memoized-ancestors-of (target)
  "Memoized version of ancestors-of."
  (or (gethash target *memoized-ancestors*)
      (setf (gethash target *memoized-ancestors*)
            (ancestors-of target))))

(defun propagate-event (event target)
  "Propagate EVENT to TARGET and its ancestors, following capturing and bubbling phases."
  (let ((propagation (make-event-propagation)))
    (labels ((capturing-phase (target)
               (dolist (ancestor (memoized-ancestors-of target))
                 (when (not (event-propagation-stopped propagation))
                   (invoke-listeners event ancestor :capturing propagation)
                   (capturing-phase ancestor))))
             (bubbling-phase (target)
               (dolist (ancestor (memoized-ancestors-of target))
                 (when (not (event-propagation-stopped propagation))
                   (invoke-listeners event ancestor nil propagation)
                   (bubbling-phase ancestor)))))
      ;; Capturing phase.
      (capturing-phase target)
      ;; Target phase.
      (invoke-listeners event target nil propagation)
      ;; Bubbling phase.
      (setf (event-propagation-capturing propagation) nil)
      (bubbling-phase target))))

  ;; Remove event delegation for a specific event, target, condition, and callback
(defun remove-event-delegation (event target condition callback)
  "Remove event delegation for a specific event, target, condition, and callback"
  (let ((delegation (make-event-delegation :event event
                                           :target target
                                           :condition condition
                                           :callback callback)))
    (setf (event-delegations event) (remove delegation (event-delegations event)))
    (setf (event-delegations target) (remove delegation (event-delegations target)))))

;; Utility functions for event propagation and delegation management
(defun event-target-matches-condition-p (event target condition)
  "Check if the event target matches the specified condition for event delegation.
    The condition can be a string, a list of strings, or a function."
  (cond
    ((stringp condition)
     (string= (event-target-id target) condition))
    ((listp condition)
     (member (event-target-id target) condition))
    ((functionp condition)
     (funcall condition event target))))


  ;; Check if the event should propagate (not stopped by stopPropagation)
(defun event-should-propagate-p (event target)
  "Check if the event should propagate (not stopped by stopPropagation)"
  (not (event-propagation-stopped (event-propagation event target))))

(defpackage :clevelib.propagation
  (:use :common-lisp)
  (:export :propagate-event
           :stop-propagation
           :stop-immediate-propagation))

(in-package :clevelib.propagation)

;;; Event propagation data structure.
(defstruct event-propagation
  (capturing t :type boolean) ; Whether the event is in capturing phase.
  (stopped nil :type boolean) ; Whether the propagation is stopped.
  (immediate-stopped nil :type boolean)) ; Whether immediate propagation is stopped.


;;; Stop the propagation of the current event.
(defun stop-propagation (propagation)
  "Stop the propagation of the current event, given the PROPAGATION data structure."
  (setf (event-propagation-stopped propagation) t))

;;; Stop the immediate propagation of the current event.
(defun stop-immediate-propagation (propagation)
  "Stop the immediate propagation of the current event, given the PROPAGATION data structure."
  (setf (event-propagation-immediate-stopped propagation) t))

;;; Invoke the event listeners of a target during event propagation.
(defun invoke-listeners (event target phase propagation)
  "Invoke the event listeners of TARGET for EVENT during the specified PHASE, with the given PROPAGATION data structure."
  (let ((listeners (get-event-listeners target phase)))
    (dolist (listener listeners)
      (unless (event-propagation-immediate-stopped propagation)
        (funcall listener event target propagation))
      (when (event-propagation-stopped propagation)
        (return)))))
