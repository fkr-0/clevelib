(in-package :cl-user)
(uiop:define-package :clevelib
  (:nicknames :cleve)
  (:use :cl)
  (:shadow log4cl:layout)
  (:import-from :log4cl :log-error :log-info :log-debug )
  (:import-from :clevelib.relay :relay :sink-handlers )
  (:import-from :clevelib.message :message ))

(in-package :clevelib)

(cl-reexport:reexport-from :clevelib.config)
(cl-reexport:reexport-from :clevelib.mutex)
(cl-reexport:reexport-from :clevelib.event)
(cl-reexport:reexport-from :clevelib.message)
(cl-reexport:reexport-from :clevelib.channel)
(cl-reexport:reexport-from :clevelib.queue)
(cl-reexport:reexport-from :clevelib.cv)
(cl-reexport:reexport-from :clevelib.thread)
(cl-reexport:reexport-from :clevelib.thread-pool)
(cl-reexport:reexport-from :clevelib.target)
(cl-reexport:reexport-from :clevelib.event-loop)
(cl-reexport:reexport-from :clevelib.emitter)
(cl-reexport:reexport-from :clevelib.translator)
(cl-reexport:reexport-from :clevelib.relay)

;; Define your project functionality here...

(defun help ()
  (format t "~&Usage:

  clevelib [name]~&"))



(defmacro on (relay event-type event-target
               (&key (event-var 'ev) (payload-var 'payload) (async-var 'async-q))
               &body callback-body)
  "Defines a callback for a specified event on a given target.

   Arguments:
   - relay: The event relay system or object.
   - event-type: A keyword symbol specifying the event type.
   - payload-var: Optional. The variable name to bind the payload of the event. Default is 'payload'.
   - target-var: Optional. The variable name to bind the target of the event. Default is 'target'.
   - async-var: Optional. The variable name to bind the callback for enqueuing async tasks.
Default is 'async-q'.

   Returns:
   - A function that processes the specified event type.

   Example:
   (on-event my-relay :my-event
     :payload-var 'event-payload
     :target-var 'event-target
     (print event-payload))"

  `(let (;(async-symbol (gensym "async"))
          (handler-dict (or (gethash ,event-type (sink-handlers ,relay))
                          (make-hash-table :test 'equal)))
          (event-callback (lambda (message)
                            (log-debug "Handling message ~a" message)
                            (let ((,async-var (async-handle message))
                                   (,payload-var (payload message))
                                   (,event-var  message))
                              (declare (ignorable ,async-var ,payload-var ,event-var))
                              (progn ,@callback-body)))))
     (log-debug "Adding handler for message type ~a, target ~a" ,event-type ,event-target)
     (clevelib.emitter:append-hash handler-dict  event-callback ,event-target)
     (setf (gethash ,event-type (sink-handlers ,relay)) handler-dict)))


;; (defmethod clear-handlers ((relay relay))
;;   (setf (sink-handlers relay) (make-hash-table :test 'equal)))


;; (defclass sample-emit (emitter) ())
;; (defclass sample-relay (relay) ())
;; (defvar em nil)
;; (defvar rm nil)
;; (setf em (make-instance 'sample-emit))
;; (setf rm (make-instance 'sample-relay))

;; (clear-handlers rm)
;; (on rm :tick (:target-var '() :payload-var wurm)
;;   (log-info "Tick ~A" wurm)
;;   (sleep 1)
;;   (when (< 0 wurm) (make-message :tick (decf wurm))))
;; (enqueue-message rm (make-message :tick 5)
;; (run (relay-message-loop rm))
;; (gethash :tick (sink-handlers rm) )
