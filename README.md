![https://github.com/fkr-0/clevelib/blob/media/media/logo.png?raw=true](https://github.com/fkr-0/clevelib/blob/media/media/logo.png?raw=true)
# Clevelib: An Event-Message System Library

Clevelib is an API-driven library designed to facilitate the implementation of asynchronous event-message systems (EMS). The project is organized into three main areas:
1. **API Functions**: These functions lay out the event system basics.
2. **EMS/Message/Event**: These refine API implementations into a usable async event-message-system.
3. **Primitives**: These include necessary components for implementation such as thread pools, event loops, queues, and async tasks.

## Table of Contents

- [API Functions](#api-functions)
  - [Events](#events)
  - [Event Handling and Emitting](#event-handling-and-emitting)
  - [Event to Message Translation](#event-to-message-translation)
  - [Messaging](#messaging)
  - [Message Handling and Relaying](#message-handling-and-relaying)
  - [Targeting Messages](#targeting-messages)
  - [Propagation Mechanisms](#propagation-mechanisms)
  - [Async Execution and Response Handling](#async-execution-and-response-handling)
- [EMS/Message/Event Implementations](#ems-message-event-implementations)
  - [Example EMS](#example-ems)
- [Primitives](#primitives)

## API Functions

### Events

Events are basic units of the system, which carry a type.

```common-lisp
(defgeneric event-type (event)
  (:method ((ev t)) :untyped-event)
  (:documentation "An event has a type."))
```

### Event Handling and Emitting

Emitters emit events that are handled by sinks. Emission involves calling the sink's `on-event` method.

```common-lisp
(defgeneric on-event (event-handler event &rest args)
  (:documentation "Handle an event."))

(defgeneric connect-sink (emitter sink event-type &rest args)
  (:documentation "Connect a sink to the emitter."))

(defgeneric connected-sinks (emitter event-type &rest args)
  (:documentation "Return the sinks connected to the emitter."))

(defgeneric emit (emitter event &rest args)
  (:documentation "Emit an event."))

(defmethod emit ((emitter t) event &rest args)
  (declare (ignorable args))
  (dolist (sink (connected-sinks emitter event))
    (handler-case
      (on-event sink event)
      (error (e)
        (format t "Error handling event ~A: ~A~%" event e)))))
```

### Event to Message Translation

Translate events to messages for the message system.

```common-lisp
(defgeneric translate-event-to-message (event-message-system event &rest args)
  (:documentation "Translate an event to a message."))
```

### Messaging

Messengers can receive messages. An EMS can act both as a messenger and as a message system.

```common-lisp
(defgeneric on-message (messenger message &rest args)
  (:documentation "Handle a message."))
```

### Message Handling and Relaying

Relay messages within the EMS, either broadcasting or targeting specific messengers.

```common-lisp
(defgeneric relay-message (message-system message &rest args)
  (:documentation "Relay a message."))

(defgeneric connected-messengers (message-system &rest args)
  (:documentation "Return the messengers connected to the message-system."))

(defmethod relay-message ((broadcasting-message-system t) message &rest args)
  (declare (ignorable args))
  (dolist (messenger (connected-messengers broadcasting-message-system))
    (handler-case
      (on-message messenger message)
      (error (e)
        (format t "Error relaying message ~A: ~A~%" message e)))))
```

### Targeting Messages

Target messages to specific messengers or broadcast them.

```common-lisp
(defgeneric target-messengers (message &rest args)
  (:documentation "Return the target messengers of the message.")
  (:method ((message t) &rest args)
    (declare (ignorable args message))
    nil))

(defgeneric set-target-messengers (message new-target-messengers &rest args)
  (:documentation "Set the target messengers of the message."))

(defmethod (setf target-messengers) (new-target-messengers (message t))
  (set-target-messengers message new-target-messengers))

(defmethod broadcast-p ((message t) &rest args)
  (declare (ignorable args))
  (or
    (not (target-messengers message))
    (eq (target-messengers message) :broadcast)))

(defmethod relay-message ((targeting-message-system t) (targeting-message t) &rest args)
  (declare (ignorable args))
  (if (target-messengers targeting-message)
    (dolist (messenger (target-messengers targeting-message))
      (on-message messenger targeting-message))
    (dolist (messenger (connected-messengers targeting-message-system))
      (on-message messenger targeting-message)))
  (values nil))
```

### Propagation Mechanisms

Handle message propagation with capture, bubble, and inhibit mechanisms.

```common-lisp
(defgeneric propagation-stopped-p (message &rest args)
  (:documentation "Return T if the propagation of the message should be stopped."))

(defgeneric capture-p (message &rest args)
  (:documentation "Return T if the message should be captured."))

(defgeneric bubble-p (message &rest args)
  (:documentation "Return T if the message should be bubbled."))

(defgeneric get-parent (nested-messenger)
  (:documentation "Return the parent of the nested messenger, or NIL if it is the root."))

(defgeneric get-children (nested-messenger)
  (:documentation "Return the children of the nested messenger, or NIL if it has none."))

(defgeneric root-to-target-path (nested-messenger)
  (:documentation "Some means of obtaining the order of messengers from root
to target that we use to facilitate the propagation of events."))

(defmethod target-to-root-path ((target t))
  (let ((path nil)
         (current target))
    (loop
      (push current path)
      (let ((parent (get-parent current)))
        (if parent
          (setf current parent)
          (return path))))))

(defmethod root-to-target-path ((target t))
  (target-to-root-path target))

(defgeneric ems-root (ems &rest args)
  (:method ((ems t) &rest args)
    (declare (ignorable args ems))
    (when (connected-messengers ems)
      (ems-root (car (connected-messengers ems)))))
  (:documentation "Return the root of the ems."))

(defmethod broadcast (ems root message &rest args)
  (declare (ignorable args))
  (set-target-messengers message :broadcast)
  (let ((children (get-children root)))
    (dolist (child children)
      (on-message child message :state :broadcast)
      (broadcast ems child message))))

(defmethod relay-message ((propagating-message-system t) (propagating-message t) &rest args)
  (declare (ignorable args))
  (if (broadcast-p propagating-message)
    (progn
      (on-message (ems-root propagating-message-system) propagating-message :state :broadcast)
      (broadcast propagating-message-system (ems-root propagating-message-system) propagating-message))
    (let ((trgs (if (listp (target-messengers propagating-message))
                  (target-messengers propagating-message)
                  (list (target-messengers propagating-message)) )))
      (dolist (trg trgs)
        (let ((path (root-to-target-path trg)))
          (when (capture-p propagating-message)
            (dolist (node (butlast path))
              (unless (propagation-stopped-p propagating-message)
                (on-message node propagating-message :state :capture))))
          (unless (propagation-stopped-p propagating-message)
            (on-message (car (last path)) propagating-message :state :at-target))
          (when (bubble-p propagating-message)
            (dolist (node (cdr (reverse path)))
              (unless (propagation-stopped-p propagating-message)
                (on-message node propagating-message :state :bubble)))))))))
```

### Async Execution and Response Handling

Handle asynchronous execution of tasks and responses within the EMS.

```common-lisp
(defgeneric async-exec (message task &rest args))
(defgeneric respond (message new-message &rest args))

(defgeneric add-async-exec-handler(ems message &rest args))
(defgeneric add-response-handler(ems message &rest args))
```

## EMS/Message/Event Implementations

### Example EMS

Implementing a simple EMS to demonstrate the usage of the API.

```common-lisp
(defclass simple-sink () ((events :initform nil :initarg :events :accessor events)
                           (message-system :initform nil :initarg :message-system :accessor message-system)))
(defclass simple-emitter () ((sinks :initform nil :initarg :sinks :accessor sinks)))

(defmethod connected-sinks ((emitter simple-emitter) event-type &rest args)
  (declare (ignorable args))
  (sinks emitter))

(defmethod on-event ((sink simple-sink) event &rest args)
  (declare (ignorable args))
  (push event (events sink))
  (format t "Event received: ~A (~A)~%" event (

events sink))
  (if (message-system sink)
    (on-message (message-system sink)
      (translate-event-to-message
        (message-system sink)
        event))))

(defmethod emit ((emitter simple-emitter) event &rest args)
  (declare (ignorable args))
  (dolist (sink (connected-sinks  emitter (event-type event)))
    (handler-case
      (on-event sink event)
      (error (e)
        (format t "Error handling event ~A: ~A~%" event e)))))

(defclass simple-translator () ())

(defmethod translate-event-to-message ((translator t) event &rest args)
  (declare (ignorable args))
  (list 'message event))

(defclass simple-receiver () ())

(defmethod on-message ((receiver simple-receiver) message &rest args)
  (declare (ignorable args))
  (format t "Message received: ~A~%" message))

(defclass simple-message-system ()
  ((messengers :initform nil :initarg :messengers :accessor messengers)))

(defmethod connected-messengers ((system simple-message-system) &rest args)
  (declare (ignorable args))
  (messengers system))
```

## Primitives
TODO
