
(defpackage :clevelib.event-loops
  (:use #:cl #:bt #:clevelib.queue  :clevelib.threads :clevelib.event-system
    )
  (:export #:enqueue-event
    #:make-event-loop
    #:async
    #:stat-loop
    #:defloop
    #:thread
    #:active
    #:until
    #:with-event-loop
    #:lock
    #:fps
    #:get-event-loop
    #:thread
    #:loop-fun
    #:event-loop-active-p
    #:notify-event
    #:event-loop
    #:enqueue-event
    #:dequeue-event
    #:process-single-event
    #:trigger-error-event
    #:process-loop-events
    #:start-event-loop
    #:state
    #:stop-event-loop
    #:toggle-event-loop
    #:error-event-handler
    )
  (:import-from :cl.state :*state*))

(in-package #:clevelib.event-loops)

(defclass evloop ()
  ((i-queue :initarg :i-queue
     :initform (make-queue)
     :accessor i-queue
     :documentation "The input queue of the event loop.
The loop function is called with the event as argument.")
    (state
      :initform :initialized
      :accessor state
      :documentation "Keyword describing the state of the event loop.
Possible values are :initialized, :running, :stopped and :error.")
    (o-queue :initarg :o-queue
      :initform (make-queue)
      :accessor o-queue
      :documentation "The output queue of the event loop. The return value
of the loop function is enqueued here if it is not equal nil.")
    (thread :initarg :thread
      :initform nil
      :accessor thread
      :documentation "The thread that executes the event loop.")
    (fps :initform nil
      :initarg :fps
      :accessor fps)
    (wait :initform t :accessor wait
      :initarg :wait
      :documentation "If true, the event loop will wait for an event to be enqueued,
otherwise it will return nil if no event is available.")
    (loop-function :initarg :loop-function
      :initform (lambda (event)
                  (format t "Received ~A. No loop function defined.~%" event))
      :accessor loop-function
      :initarg :loop-fun
      :documentation "The function that is executed
in the event loop thread."))
  (:documentation "An event loop."))

(defmethod stat-loop ((evloop evloop))
  "Return plist of stats for each loop in the event system."
  (list (sb-thread:thread-name (slot-value evloop 'CLEVELIB.EVENT-LOOPS:THREAD))
    (slot-value evloop 'CLEVELIB.EVENT-LOOPS:STATE)))


(defmacro with-fps (fps &body body)
  "Execute the body with the given fps."
  `(format t "FPS: ~A~%" ,fps)
  (if (and (not (null fps)) (numberp fps))
    `(let ((fpsreal (/ 1000000.0 ,fps))
            (time (get-internal-real-time)))
       (progn ,@body)
       (let ((time-diff (- (get-internal-real-time) time)))
         (if (< time-diff fpsreal)
           ;; (progn
           ;;   (format t "Sleep: ~A ~%" (- fpsreal time-diff))
           (sleep (/ (- fpsreal time-diff) 1000000.0))
           (warn "FPS too low! ~A" time-diff))))
    `(progn ,@body)))

(defmethod run ((evloop evloop))
  (setf (state evloop) :running)
  (loop
    (with-fps (fps evloop)
      (let* ((ev (dequeue (i-queue evloop) (wait evloop)))
              (r (funcall (loop-function evloop) ev))
              (o-l (o-queue evloop)))
        (when (and o-l r)
          (enqueue o-l r)
          )
        (when (eq ev :quit)
          (format t "~A: Quit event received.~%" (thread evloop))
          (setf (state evloop) :stopped)
          (return-from run))
        )
      )
    ))

;; TODO typecase and proper quit event class
(defmethod stop ((evloop evloop))
  "Stop the event loop by enqueuing the :quit event.
The loop will process events until the :quit event
is dequeued. It will dispatch it and terminate afterwards"
  (when (eq (state evloop) :running)
    (enqueue (i-queue evloop) :quit)))

(defmacro defloop (name event-arg (&key
                                    (run t )
                                    (input-queue nil )
                                    (output-queue nil )
                                    (wait t )
                                    (fps nil )
                                    (mgr nil ))
                    &body body)
  "Define an event loop macro with the given name and @BODY operating on
  events EVENT-ARG dequeued from INPUT-QUEUE and enqueuing result values != NIL
  on OUTPUT-QUEUE.
  
  Allows specifying an event argument, optional run flag, input queue, 
  output queue, wait flag, frames per second, and manager.
  The body is executed in the event loop thread. The event argument
  is passed to the body.

  Arguments:
  NAME: The name of the event loop.
  EVENT-ARG: The name of the event argument.
  optional keyword arguments:
  RUN: If true, the event loop is started immediately.
  INPUT-QUEUE: The input queue of the event loop. If not specified a
               new queue instance is created and attached. @BODY operates on
               EVENT-ARG dequeued from INPUT-QUEUE.
  OUTPUT-QUEUE: The output queue of the event loop. If not specified a
               new queue instance is created and attached. The last or 
               returned value of @BODY is enqueued to output-queue if it does not
               equal NIL.
  WAIT: If true, the event loop will wait for an event to be enqueued,
        otherwise it will return NIL if no event is available.
  FPS: The frames per second of the event loop.
  MGR: EVENT-SYSTEM that manages the event loop. Defining the mgr
       allows accessing the loop as member variable and defines helpers implemented
       as methods dispatching on the Event-System class and allow accessing the loop and
       thread through name-based methods.
  @BODY: The body of the event loop. It is executed in the event loop thread.
         The event argument is passed as EVENT-ARG to the body.

  The macro body specifies depending on NAME:
    Methods (all dispatching on MGR):
    - NAME-RUN: Run the event loop.
    - NAME-STOP: Stop the event loop by enqueuing an :QUIT event and
                 allowing @BODY to process it in its last iteration.
    - NAME-OUT-QUEUE: Return the output queue of the event loop.
    - NAME-IN-QUEUE: Return the input queue of the event loop.
    - NAME-LOOP: Return the event loop.
    - NAME-THREAD: Return the thread of the event loop.
    - NAME-STATE: Return the state of the event loop: One of keywords:
                  :init :running :error :stopped"

  ;; Generate internal symbols
  (let ((thread-sym (intern (string-upcase (format nil "~A-LOOP" name))))
         (run-meth (intern (string-upcase (format nil "~A-run" name))))
         (stop-meth (intern (string-upcase (format nil "~A-stop" name))))
         (out-queue-sym (intern (string-upcase(format nil "~A-out-queue" name))))
         (in-queue-sym (intern (string-upcase(format nil "~A-in-queue" name))))
         (state-sym (intern (string-upcase(format nil "~A-state" name))))
         (rendered-fun `(lambda (,event-arg) ,@body)))

    ;; Define methods on generated symbols
    `(progn
       ;; Create loop instance
       (let ((inst (make-instance 'evloop :wait ,wait :fps ,fps :loop-fun ,rendered-fun)))

         ;; Define run method
         (defgeneric ,run-meth (sys))
         (defmethod ,run-meth ((sys clevelib.event-system:event-system))

           ;; Start thread running loop
           (clevelib.event-system:idempotent-add-loops sys inst)
           (setf  (thread inst)
             (start-thread (clevelib.event-system:event-system-pool sys)
               (lambda ()(run inst))
               :description ,(format nil "~A-LOOP" name))))

         ;; Define stop method
         (defgeneric ,stop-meth (sys))
         (defmethod ,stop-meth((sys clevelib.event-system:event-system)))
         (stop inst)

         ;; Accessors
         (defmethod ,out-queue-sym((sys clevelib.event-system:event-system))
           (o-queue inst))
         (defmethod ,in-queue-sym((sys clevelib.event-system:event-system))
           (i-queue inst))
         (defmethod ,thread-sym((sys clevelib.event-system:event-system))
           inst)
         (defmethod ,state-sym((sys clevelib.event-system:event-system))
           (state inst))

         ;; Set queues if provided
         (when ,input-queue
           (setf (i-queue inst) ,input-queue))
         (when ,output-queue
           (setf (o-queue inst) ,output-queue))

         ;; Ignore temporaries
         `(declare (ignorable run-meth thread-sym stop-meth out-queue-sym in-queue-sym state-sym))

         ;; Run if specified
         (when ,(and run (not (null mgr)) `(,run-meth ,mgr)))))))

