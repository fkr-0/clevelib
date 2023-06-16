;;;; clevelib/macros.lisp

;; This file contains utility macros that simplify the usage of the event
;; system when interfacing with other parts of the project. These macros
;; provide a more convenient and readable syntax for common tasks such as
;; registering event listeners, triggering events, and managing live updates.

(defpackage :clevelib.macros
  (:use :cl :bt :clevelib.core )
  (:export :deflistener
    :with-event-loop
    :trigger
    :on
    :defevent
    :trigger-event
    :live-update
    :defasync
    :with-mutex
    :bind
    :with-condition-variable))


(in-package :clevelib.macros)
;; (defconstant +event-handler+ (make-hash-table))
;; (defconstant +live-updates+ (make-hash-table))
;;;; clevelib/macros.lisp

;; (let ((+event-handler+ (make-hash-table)))  ;Use let instead of defconstant
;;   (let ((+live-updates+ (make-hash-table)))

;; Macro: on
(defmacro on (event target &body body)
  "Binds a callback to an event on a target.
   EVENT is a keyword symbol specifying event type
   and TARGET is a symbol. Body is the body of a callback function
   that got the triggered event bound to parameter event passed."
  `(add-event-listener ,event ,target (lambda (event) ,@body)))  ;Use lambda instead of defun

;; Macro: trigger
(defmacro trigger (event target &rest args)
  "Triggers an event on a target.
   EVENT is a keyword symbol specifying event type
and TARGET is a symbol."
  `(dispatch-event (make-event ,event ,target :data ,args)))

;; Generic binding function
(defun bind (target event &optional callback)
  "Binds a callback to an event on a target.
   If no callback is provided, returns a closure. The closure can be
   called with a callback to bind to the event."
  (if callback
    (add-event-listener event target callback)
    (lambda (clb) (add-event-listener event target clb))))
;; Macro: defevent
;; (defmacro defevent (name event target &body body)
;;   "Defines an event handler function and binds it to an event."
;;   `(progn
;;      (defun ,name ()
;;        ,@body)
;;      (on ,event ,target #',name)))


;; Usage examples
;; (apply #'bind button :click (lambda () (print "Clicked!"))) ;Use apply instead of funcall
;; (let ((click-handler (bind list :select)))
;;   (apply click-handler (lambda () (print "Selected!"))))
;; (defevent handle-focus :focus 'textarea
;;   (print "Focused!"))
;; (on :submit form (print "Submitted!"))
;; (trigger :open window)
;;(trigger :focus 'textarea)
;; Binds a callback to an event on a target
;; ;; Usage examples
;; (bind button :click (lambda () (print "Clicked!")))
;; (let ((click-handler (bind list :select)))
;;   (funcall click-handler (lambda () (print "Selected!"))))
;; (defevent handle-focus :focus textarea
;;   (print "Focused!"))
;; (on :submit form (print "Submitted!"))
;; (trigger :open window)

;; Macro: deflistener
;; Purpose: Define an event listener function and register it to a target.
(defmacro deflistener (name event target &body body)
  (let ((event-sym (gensym "EVENT-"))
         (target-sym (gensym "TARGET-")))
    `(progn
       (defconstant ,event-sym ',event)
       (defconstant ,target-sym ',target)
       (defun ,name (,event-sym)
         ,@body)
       (add-event-listener ,event-sym ,target-sym #',name))))

;; Macro: with-event-loop
;; Purpose: Run the body of code within a specific event loop.
(defmacro with-event-loop (event-loop &body body)
  (let ((event-loop-sym (gensym "EVENT-LOOP-")))
    `(let ((,event-loop-sym ,event-loop))
       (set-event-loop ,event-loop-sym)
       ,@body)))

;; Macro: live-update
;; Purpose: Define a live update function with optional interval.
(defmacro live-update (update-function &key interval)
  "Defines a live update function with optional interval."
  (let ((update-function-sym (gensym "UPDATE-FUNCTION-")))
    `(defconstant ,update-function-sym ',update-function)
    `(setf (gethash ,update-function-sym +live-updates+) ,interval)))

;; Macro: defasync
;; Purpose: Define an asynchronous function.
(defmacro defasync (name &body body)
  "Defines an asynchronous function."
  (let ((func (gensym "FUNC-")))
    `(defun ,name (&rest args)
       (let ((,func (function (lambda () ,@body))))
         (apply #'async-exec ,func args)))))

;; Macro: with-mutex
;; Purpose: Run the body of code while holding the given mutex.
(defmacro with-mutex (mutex &body body)
  "Run the body of code while holding the given mutex."
  `(progn
     (bt:with-lock-held (,mutex)
       ,@body)))
;; Macro: with-condition-variable
;; Purpose: Run the body of code while waiting for the given condition variable.
(defmacro with-condition-variable ((condition-variable lock) &body body)
  "Run the body of code while waiting for the given condition variable."
  `(progn
     (bt:with-lock-held (,lock)
       (bt:condition-wait ,condition-variable ,lock)
       ,@body)))
