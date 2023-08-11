;; TODO generalize event handling:

;; (defstruct condition
;;   event-type       ; e.g., :click, :keydown
;;   target-element   ; e.g., a specific DOM element or identifier
;;   phase            ; e.g., :bubbling, :capturing, :at-target
;;   custom-check     ; a lambda for custom conditions
;; )
;; (defun make-condition (&key event-type target-element phase custom-check)
;;   (make-instance 'condition :event-type event-type
;;                              :target-element target-element
;;                              :phase phase
;;                              :custom-check custom-check))

;; (make-condition :event-type :click :target-element some-element :phase :bubbling)
;; (make-condition :event-type :keydown :custom-check (lambda (event) ...))
(in-package :clevelib.event-system)

(defclass event-handler ()
  ((event-type :initarg :event-type
     :accessor event-handler-event-type
     :documentation "The type of event this handler is for.")
    (callback :initarg :callback
      :accessor event-handler-callback
      :documentation "The function to call when the event is triggered. A handlers
callback function is called with the event as its first argument, and the optional data as its second argument.")
    (target :initarg :target
      :accessor event-handler-target
      :documentation "The target of the event handler.")
    (options :initarg :options
      :accessor event-handler-options
      :documentation "The options for the event handler."))
  (:documentation "An event handler is a function that is assigned to a target object.
It is called when an event is triggered with the target as its destination. Furthermore,
the event handler can be called during the capturing or bubbling phase of the event propagation,
or both, and it can be called only once or multiple times. Relevant behavior is specified in the options.
Currently implemented and supported options are:
:capturing - call handler during capturing phase, (e.g. when triggering an event on an object such as a button,
the button's handlers are called first, then the button's parent's handlers, and so on until the root is reached)
:at-target - call handler at target object, (e.g. when triggering an event on an object such as a button,
the button's handlers are called first, then the button's parent's handlers, and so on until the root is reached)
:bubbling - call handler during bubbling phase, (e.g. when triggering an event on an object such as a button,
the bubble phase starts at the root and goes down to the button, the target object. from there, the capture phase starts,
going up to the root again)
:once - call handler once during capturing or bubbling phase, (e.g. when triggering an event on an object,
the object's handlers are called once, then removed)
:once-capturing - call handler once during capturing phase, (e.g. when triggering an event on an object,
the object's handlers are called once, then removed)
:once-bubbling - call handler once during bubbling phase, (e.g. when triggering an event on an object,
the object's handlers are called once, then removed)"))

;; ;; options: :capturing - call handler during capturing phase, (e.g. when triggering an event on an object such as a button,
;;  ;; the button's handlers are called first, then the button's parent's handlers, and so on until the root is reached)
;; ;; :at-target - call handler at target object, (e.g. when triggering an event on an object such as a button,
;; ;; the button's handlers are called first, then the button's parent's handlers, and so on until the root is reached)
;;  ;; :bubbling - call handler during bubbling phase, (e.g. when triggering an event on an object such as a button,
;;  ;; the bubble phase starts at the root and goes down to the button, the target object. from there, the capture phase starts,
;;  ;; going up to the root again)
;; ;;         :once - call handler once during capturing or bubbling phase, (e.g. when triggering an event on an object, the object's handlers are called once,
;; ;;         then the object's parent's handlers are called once, and so on until the root is reached. if the event is triggered again, the handlers are not called
;; ;;         again and the event is not propagated further)
;;  ;; :capture-once - call handler once during capturing phase, (e.g. when triggering an event on an object,
;;  ;; the capture phase, the phase that goes from the root to the target object, is started. the object's handlers are called once,
;;  ;; on the next call to trigger, the handlers are not called again and the event is not propagated further)
;;  ;; :bubble-once - call handler once during bubbling phase, (e.g. when triggering an event on an object, the bubble phase,
;;  ;; the phase that goes from the target object to the root, is started. the object's handlers are called once, on the next call to trigger,
;;  ;; the handlers are not called again and the events bubble phase is not started)
;; ;;         :capture-first - call handler first during capturing phase, (e.g. when triggering an event on an object,
;; ;; :bubble-first - call handler first during bubbling phase,
;; ;;         :capture-last - call handler last during capturing phase,
;;  ;; :bubble-last - call handler last during bubbling phase, (e.g. when triggering an event on an object,
;;  ;; the object's handlers are called first, then the object's parent's handlers, and so on until the root is reached)
;; ;;         :capture-only - only call handler during capturing phase, (e.g. :capture-only t)
;; ;; :bubble-only - only call handler during bubbling phase, (e.g. :capture-only t)
;; ;;         :capture-all,
;;  :bubble-all
