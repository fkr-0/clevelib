;; Event handling library - Api
;;
;;
;; This library provides a simple event handling mechanism. It is based on the
;; following concepts:
;; 1. Event emitters - objects that can emit events
;; 2. Event targets - objects that can receive events
;; 3. Event sinks - objects that can receive events and are responsible for
;;   dispatching them to targets
;;
;; In order to use the library, the following steps are required:
;; 1. Subclass event-target, event-emitter, event-sink. Override the generic
;;  functions as needed. A dispatching mechanism is deliberately not provided
;;  as it is application specific.
;;  2. Create instances of the classes.
;;  3. Connect the sink to the emitter.
;;  4. Emit events.
;;
;; Note that the individual elements are decoupled.
;; they are fully exchangeable as long as the following mechanisms are implemented
;; on the exchanging structure:
;; Sink
;; - connect-target <sink> <target>: registers a target with the sink to allow calling
;;  handlers of the target for specific event types
;; - sink-event-fn <sink> <event-type>: returns a function that can be called to
;;   emit an event of type <event-type> to the sink
;; - dispatch-event <sink> <event>: dispatches an event to registered targets
;; Emitter
;; - emit: emits an event
;; - link-sink <emitter> <sink> <event-type>: links a sink to the emitter for a specific
;;   event type
;; Target
;; - add-handler <target> <event-type> <handler> <priority>: adds a handler for a specific
;;  event type with a specific priority (sink needs to use the connection to obtain
;;  knowledge about certain handlers)
;;
;; Usage example
;; 1. Subclass event-target, event-emitter, event-sink:
;; (defclass my-target (event-target) ())
;; (defclass my-emitter (event-emitter) ())
;; (defclass my-emitting-target (my-target my-emitter) ()) ;; a target that can emit events
;; (defclass my-sink (event-sink) ())
;; (defclass event () ((event-type :accessor event-type :initarg :event-type :initform nil))) ;; event class
;; implement a basic dispatching mechanism
;; (defmethod dispatch-event ((sink my-sink) (event event))
;;   (let* ((handlers (gethash (event-type event) (sink-handlers sink))
;;            (callables-with-priority
;;              (mapcar #'car
;;                (filter (lambda (x) (>= (car x) (event-priority event)))
;;                  handlers)))))
;;     (dolist (callable-with-priority callables-with-priority)
;;       (funcall callable-with-priority event))))
;; 2. Create instances of the classes:
;; (defvar snk (make-instance 'my-sink))
;; (defvar emittr (make-instance 'my-emitter))
;; (defvar trget (make-instance 'my-target))
;; 3. Connect the sink to the emitter:
;; (add-handler trget 'my-event ;; note that trget stores its handler locally as well
;;   (lambda (&key (target nil) (data nil) (priority 0))
;;     (format t "i, target ~a, do something clever with ~a ~a ~a~%"
;;       target
;;       data
;;       priority)))
;; (connect-target snk trget) ;; this will register the target and local handlers with the sink
;; (link-emitter snk emittr 'my-event) ; this will pass a sink-event function to the
                                        ; emitter allowing it to emit events to the sink
;; (emit emittr 'my-event :target trget :data "hello world")
;;
;; => i, target #<MY-TARGET {1004B0F3B3}> , do something clever with "hello world" 0 my-event
;;
;; see examples/base-example.lisp for an example implementation
;; +-----------------+     +--------------+       +----------------+
;; | Event Emitter e |     | Event Sink s |       | Event Target t |
;; +-----------------+     +--------------+       +----------------+
;;          |                     |                        |
;;          ========= Setup phase ==========================
;;          |                     |                        |
;;          |                     |                        |-----+
;;          |                     |                        |     | (add-handler
;;          |                     |                        |<----+  t 'my-event)
;;          |                     |  (connect-target s t)  |
;;          |                     |<---------------------->|
;;          |                     |   [pass registry-fn]   |
;;          |                     |----------------------->|
;;          |                     | [set-up event-handler] |
;;          |  (link-sink e s     |<-----------------------|
;;          |    'my-event)       |                        |
;;          |<------------------->|                        |
;;          | [pass emission fn]  |                        |
;;          |<--------------------|                        |
;;          ========= Event Emitter emits event ============
;;          |   (emit 'my-event)  |                        |
;;          |-------------------->|                        |
;;          |                     | (process-event s       |
;;          |                     |        'my-event)      |
;;          |                     |                        |
;;          |                     |  [call event-handler]  |
;;          |                     |----------------------->|
;;          |                     |                        | [handle event]
;;          |                     |                        | "i, target t,
;;          |                     |                        |  do something
;;          |                     |                        |  clever with
;;          |                     |                        |  'my-event"
;; +-----------------+     +--------------+       +----------------+

(defpackage :clevelib.base-system
  (:use :cl)
  (:export
    ;; base classes
    :event-sink
    :event-emitter
    :event-target
    :target-id
    :target-handlers
    :target-sinks
    ;; sink <-> emitter
    :unlink-sink ;; (emitter sink event-type) -> nil
    :link-sink ;; (emitter sink event-type) -> (lambda (event-type &key target data priority))
    :sink-event-fn ;; (sink event-type) -> (lambda (target data priority))
    ;; sink <-> target
    :add-handler
    :remove-handler
    :connect-target
    :sink-handlers
    :disconnect-target
    :dispatch-event
    :emit))

(in-package :clevelib.base-system)

(defgeneric sink-event-fn (sink event-type)
  (:documentation "Return the sink-event function for
EVENT-TYPE with PRIORITY. If no sink-event is found, return NIL
  Arguments:
    SINK - the event sink
    EVENT-TYPE - the event type
  Return value: the sink-event function (lambda (target data priority"))

(defgeneric link-sink (event-emitter emittable &rest event-type)
  (:documentation "Link an EVENT-EMITTER to an EVENT-SINK. On
emission, the EMISSION-CALLBACK function is called with the following keyword
arguments:
  :TARGET - the target of the event
  :DATA - the data of the event
  :PRIORITY - the priority of the event
The EMISSION-CALLBACK function is returned by the SINK-EVENT-FN function of the
EVENT-SINK. The EVENT-EMITTER is responsible for calling the EMISSION-CALLBACK
function with the correct arguments.
  Arguments:
    EVENT-EMITTER - the event emitter
    EMITTABLE - the event sink or the sink-event function
    EVENT-TYPE - the event type
  Return value: the event emission function (lambda (event-type &key target data priority)"))

(defgeneric unlink-sink (event-emitter emittable event-type)
  (:documentation "Unlink an EVENT-EMITTER from an EVENT-SINK
  Arguments:
    EVENT-EMITTER - the event emitter
    EMITTABLE - the event sink or the sink-event function
    EVENT-TYPE - the event type
  Return value: nil"))

(defgeneric emit (event-emitter event-type &rest args) ; &key target data priority)
  (:documentation "Emit an event of type EVENT-TYPE. The event is passed to all
linked EVENT-SINKs with the following keyword arguments:
  :TARGET - the target of the event
  :DATA - the data of the event
  :PRIORITY - the priority of the event
  Dispatching to relevant targets is the responsibility of the EVENT-SINK. The
EVENT-EMITTER is responsible for calling the SINK-EVENT-FN function of the
EVENT-SINK with the correct arguments.
  Arguments:
    EVENT-EMITTER - the event emitter
    EVENT-TYPE - the event type
    &key
      TARGET - the target of the event
      DATA - the data of the event
      PRIORITY - the priority of the event
  Return value: nil"))

(defgeneric sink-event-fn (sink event-type)
  (:documentation "Return the sink-event function for
    EVENT-TYPE with PRIORITY. The sink SINK provides this function
to the event emitter. If no sink-event is found, return NIL.
  Arguments:
    SINK - the event sink
    EVENT-TYPE - the event type
  Return value: the sink-event function (lambda (target data priority"))

;; (defgeneric add-handler (target event-type handler)
;;   (:documentation "Add a handler to event-target TARGET
;; for EVENT-TYPE. If sinks are connected they are notified. Handler can be,
;; in its simplest form, a callable/function. It may also be an instance of a
;; class for which a means to call a handler function is provided. On handling
;; an event, the handling function / method / class instance is passed the relevant
;; event, from which any further information can be obtained.
;;   Arguments:
;;     TARGET - the event target
;;     EVENT-TYPE - the event type
;;     HANDLER - the handler function
;;   Return value: nil
;;   Side effects: the handler is added to the target and the sinks are notified
;;   Examples:
;;     (add-handler trget 'my-event
;;       (lambda (event)
;;         (format t \"i, target ~a, do something clever with ~a ~a ~a~%\"
;;           (event-target event)
;;           (event-data event)
;;           (event-priority event)"))

;; (:method ((target event-target)
;;            (event-type symbol)
;;            (callback function))
;;   "Add a function callback handler for the given event type to the target."
;;   ;; implementation goes here
;;   )
;; (:method ((target event-target)
;;            (event-type symbol)
;;            (handler simple-handler))
;;   "Add a simple callback-only handler for the given event type to the target."
;;   ;; implementation goes here
;;   ))




(defgeneric remove-handler (target event-type handler)
  (:documentation "Remove a handler from event-target TARGET
  for EVENT-TYPE with PRIORITY. If sinks are connected they are notified.
  Arguments:
  TARGET - the event target
  EVENT-TYPE - the event type
  HANDLER - the handler function
  &key
  PRIORITY - the priority of the handler
  Return value: nil
  Side effects: the handler is removed from the target and the sinks are notified
  Examples:
  (remove-handler trget 'my-event
    (lambda (&key (target nil) (data nil) (priority 0))
      (format t \"i, target ~a, do something clever with ~a ~a ~a~%\"
        target
        data
        priority"))

(defgeneric connect-target (sink event-target)
  (:documentation "Register EVENT-TARGET as potential handler and targets for
        events. Passes a handle to EVENT-TARGET that allows registering handlers on
        specific (tye priority) pairs. When called, the EVENT-TARGET will register
        itself with the EVENT-SINK. The EVENT-SINK will then be able to call the
        EVENT-TARGET with the correct arguments.
        Arguments:
        SINK - the event sink
        EVENT-TARGET - the event target
        Return value: nil"))

(defgeneric disconnect-target (sink event-target))

(defgeneric dispatch-event (sink event)
  (:documentation "Dispatch an event to all registered targets. Dispatching
        takes place after the EMIT function has been called triggering the sink-event
        function stored with the emitter through linking."))
;; (defgeneric make-event (event-type &keys (target nil) (data nil)  (priority nil) )
(defgeneric make-event (event-type &rest args)
  (:documentation "Create an event of type EVENT-TYPE with the following
        keyword arguments:
        :TARGET - the target of the event
        :DATA - the data of the event
        :PRIORITY - the priority of the event
        Return value: the event
        Examples:
        (make-event 'my-event :target trget :data \"hello world\)"))
(defgeneric event-type (event)
  (:documentation "Return the event type of EVENT"))
(defgeneric event-target (event)
  (:documentation "Return the target of EVENT"))
(defgeneric event-data (event)
  (:documentation "Return the data of EVENT"))
(defgeneric event-priority (event)
  (:documentation "Return the priority of EVENT"))

(defclass event-sink ()
  ((handlers :initform (make-hash-table :test #'equal) :accessor sink-handlers :documentation "A hash table of event handlers"))
  (:documentation "An event sink is an object that can receive events and is
          responsible for allowing handlers to register handlers processing them as well
          as the dispatching process."))



(defclass event-emitter ()
  ((hooks :initform (make-hash-table :test #'equal) :accessor emitter-hooks :documentation "A hash table of event hooks"))
  (:documentation "An event emitter is an object that can emit events. It is linked
          to event sinks, which receive the events. The event emitter is responsible for
          calling the event sinks with the correct arguments. It may request a \"sinking\" function
          from the event sink, which it can call with the correct arguments."))

(defclass event-target ()
  ((id :accessor target-id :initform (gensym) :documentation "A unique identifier for this target.")
    (sinks :accessor target-sinks :initform nil :documentation "A list of event sinks this target listens to.")
    (handlers :accessor target-handlers :initform (make-hash-table :test #'equal)
      :documentation "A hash table of event hooks that are handled by this target"))
  (:documentation "An event target is an object that can receive events. It may handle evens
          (of specific types) and may be connected to dispatching mechanisms, typically an
          event-sink. It is capable of managing its own listeners but will notify
          connected sinks on registration to allow the sinks carrying the information
          about the targets themselves."))

(defmethod add-handler ((event-target event-target) (event-type symbol) handler)
  "Add a handler for EVENT-TYPE. If sinks are connected they are notified using their
stored registry function. The handler is stored in the target's handlers hash table
under the event type.
  Arguments:
    EVENT-TARGET - the event target
    EVENT-TYPE - the event type
    HANDLER - the handler function
  Return value: nil
  Side effects: the handler is added to the target and the sinks are notified
  Examples:
    (add-handler trget 'my-event
      (lambda (event)
        (format t \"i, target ~a, do something clever with ~a %\"
          (event-target event)
          (event-data event))))"
  (let* ((handlers (or (target-handlers event-target)
                     (make-hash-table :test #'equal)))
          (handler-list (unless (and (gethash event-type handlers) (member handler (gethash event-type handlers)))
                          (concatenate 'list (gethash event-type handlers) ;; add
                            (list handler)))))
    (setf (gethash event-type handlers) handler-list)
    (dolist (modfun (target-sinks event-target))
      (funcall modfun event-type handler))))

;; (defmethod remove-handler ((target event-target)(event-type symbol) handler &key (priority 0))
;;   "Remove a handler for EVENT-TYPE with PRIORITY. If sinks are connected they are
;;           notified of the removed handler."
;;   (let* ((priohandler (list handler priority))
;;           (handlers (or (gethash (target-id target) (target-handlers target))
;;                       (make-hash-table :test #'equal)))
;;           (handler-list (when (and (gethash event-type handlers) (member priohandler (gethash event-type handlers)))
;;                           (remove priohandler (gethash event-type handlers)) ;; remove
;;                           )))
;;     (setf (gethash event-type handlers) handler-list)
;;     (dolist (modfun (target-sinks target))
;;       (funcall modfun event-type handler :priority priority))))

(defmethod sink-event-fn ((sink event-sink) (event-type symbol) )
  "Return the sink-event function for EVENT-TYPE with PRIORITY. If no sink-event is
          found, return NIL."
  (lambda (&rest args)
    (let ((ev
            (apply #'make-event (append (list event-type) args) )))
      (format t "i, event-sink ~a, do something clever with ~a ~a~%"
        sink event-type args )
      (dispatch-event sink ev))))



(defmethod connect-target ((sink event-sink) (event-target event-target) )
  "Register EVENT-TARGET as potential handler and targets for events. Passes a handle
          to EVENT-TARGET that allows registering handlers on specific (type
          priority) pairs. The EVENT-TARGET will register itself with the
          EVENT-SINK. The EVENT-SINK will then be able to call the EVENT-TARGET
          with the correct arguments. The EVENT-SINK is added to the
          EVENT-TARGET's list of sinks SINKS.
          Arguments:
          SINK - the event sink
          EVENT-TARGET - the event target
          Return value: nil
          Side effects: the EVENT-TARGET is added to the EVENT-SINK's list of
          targets and the EVENT-SINK is added to the EVENT-TARGET's list of
          sinks"
  (let ((mod-fun
          (lambda (event-type handler)
            (let* ((handlers (or (gethash
                                   (target-id event-target) (sink-handlers sink) )
                               (make-hash-table :test #'equal)))
                    (handler-list
                      (if (and (gethash event-type handlers)
                            (member handler (gethash event-type handlers)))
                        (remove handler (gethash event-type handlers)) ;; remove
                        (concatenate 'list (gethash event-type handlers) ;; add
                          (list handler)))))
              (setf (gethash event-type handlers) handler-list)))))
    (setf (gethash (target-id event-target)
            (sink-handlers sink))
      (target-handlers event-target))
    (push mod-fun (target-sinks event-target))))

(defmethod link-sink ((e event-emitter) emission-callback &rest event-types)
  "Link an EVENT-EMITTER to an EVENT-SINK using the EMISSION-CALLBACK function. The EMISSION-CALLBACK
          function is called with the following keyword arguments:
          :TARGET - the target of the event
          :DATA - the data of the event
          :PRIORITY - the priority of the event"
  (mapcar
    (lambda (event-type)(setf (gethash event-type (emitter-hooks e))
                          (concatenate 'list (gethash event-type (emitter-hooks e)) (list emission-callback))))
    event-types))

(defmethod link-sink ((e event-emitter) (s event-sink) &rest event-types)
  "Link an EVENT-SINK to an EVENT-EMITTER. The EVENT-SINK will receive all events
of type EVENT-TYPE emitted by the EVENT-EMITTER. The EVENT-SINK will be passed a
          sink-event function that it can call with the following keyword
          arguments:
          :TARGET - the target of the event
          :DATA - the data of the event
          :PRIORITY - the priority of the event
          The sink-event function is returned by the SINK-EVENT-FN function of
          the EVENT-SINK. The EVENT-EMITTER is responsible for calling the
          sink-event function with the correct arguments.

          Arguments:
          EVENT-EMITTER - the event emitter
          EVENT-SINK - the event sink
          EVENT-TYPE - the event type

          Return value: the event emission function (lambda (event-type &key
          target data priority)) that the EVENT-EMITTER can call to emit events
          to the EVENT-SINK"
  (mapcar (lambda (event-type ) (link-sink e (sink-event-fn s event-type) event-type))
    event-types))

(defmethod emit ((e event-emitter) event-type &rest args); &key (target nil) (data nil) (priority 0))
  "Emit an event of type EVENT-TYPE. The event is passed to all linked EVENT-SINKs
using the sink-event function returned by the SINK-EVENT-FN function of the
          EVENT-SINK with the following keyword arguments:
          :TARGET - the target of the event
          :DATA - the data of the event
          :PRIORITY - the priority of the event

          Dispatching to relevant targets is the responsibility of the
          EVENT-SINK. The EVENT-EMITTER is responsible for calling the
          SINK-EVENT-FN function of the EVENT-SINK with the correct arguments.

          Arguments:
          EVENT-EMITTER - the event emitter
          EVENT-TYPE - the event type
          &key
          TARGET - the target of the event
          DATA - the data of the event
          PRIORITY - the priority of the event
          Return value: nil"
  (dolist (sink-event (gethash event-type (emitter-hooks e)))
    (when sink-event
      (apply #'funcall (append (list sink-event  event-type) args)))))


(defmethod unlink-sink ((e event-emitter) emission-callback event-type)
  "Unlink an EVENT-EMITTER from an EVENT-SINK using the EMISSION-CALLBACK function.
The EMISSION-CALLBACK function will no longer be called with the following
          keyword arguments:
          :TARGET - the target of the event
          :DATA - the data of the event
          :PRIORITY - the priority of the event"
  (setf (gethash event-type (emitter-hooks e))
    (remove emission-callback (gethash event-type (emitter-hooks e)))))

;; (defmethod unlink-sink ((e event-emitter) (s event-sink) event-type)
;;   "Unlink an EVENT-SINK from an EVENT-EMITTER. The EVENT-SINK will no longer receive events of type EVENT-TYPE
;; emitted by the EVENT-EMITTER. The EVENT-SINK will no longer be passed a sink-event function that it can call
;; with the following keyword arguments:
;;   :TARGET - the target of the event
;;   :DATA - the data of the event
;;   :PRIORITY - the priority of the event
;; The sink-event function is returned by the SINK-EVENT-FN function of the EVENT-SINK. The EVENT-EMITTER
;; is responsible for calling the sink-event function with the correct arguments.
;;   Arguments:
;;     EVENT-EMITTER - the event emitter
;;     EVENT-SINK - the event sink
;;     EVENT-TYPE - the event type
;;   Return value: the event emission function (lambda (event-type &key target data priority)) that the EVENT-EMITTER
;;     can call to emit events to the EVENT-SINK"
;;   (setf (gethash event-type (emitter-hooks e))
;;     (remove emission-callback (gethash event-type (emitter-hooks e)))))

;; (defmacro defemitter (sink emitter event-type &body body)
;;   "Define a function that emits an event of type EVENT-TYPE to the EVENT-EMITTER EMITTER with the following
;; keyword arguments:
;;   :TARGET - the target of the event
;;   :DATA - the data of the event
;;   :PRIORITY - the priority of the event
;;   Arguments:
;;     SINK - the event sink
;;     EMITTER - the event emitter
;;     EVENT-TYPE - the event type
;;     &body BODY
;;   Return value: nil"
;;   `(defun ,emitter (&key (target nil) (data nil) (priority 0))
;;      (emit ,sink ,event-type :target target :data data :priority priority)
;;      ,@body))
;; (defmacro defemission (name emitter event-type &key (target nil) (data nil) (priority 0))
;;   "Define a function that emits an event of type EVENT-TYPE to the EVENT-EMITTER EMITTER with the following
;; keyword arguments:
;;   :TARGET - the target of the event
;;   :DATA - the data of the event
;;   :PRIORITY - the priority of the event
;;   Arguments:
;;     NAME - the name of the function
;;     EVENT-EMITTER - the event emitter
;;     EVENT-TYPE - the event type
;;     &key
;;       TARGET - the target of the event
;;       DATA - the data of the event
;;       PRIORITY - the priority of the event
;;   Return value: nil"
;;   `(defun ,name (&key (target ,target) (data ,data) (priority ,priority))
;;      (emit ,emitter ,event-type :target target :data data :priority priority)))
