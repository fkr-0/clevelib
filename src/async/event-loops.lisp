
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
The event loop will finish
processing the current event
and then stop."
  (when (eq (state evloop) :running)
    (enqueue (i-queue evloop) :quit)))



;; (defmethod notify-event ((ev-loop event-loop))
;;   "Notify an event loop that an event has occurred."
;;   (clevelib.threads:signal-condition (condition-var ev-loop)))

;; (defmethod enqueue-event ((ev-loop event-loop) event)
;;   "Enqueue an event in the event queue."
;;   (with-mutex (lock ev-loop)
;;     (clevelib.queue:enqueue  (queue ev-loop) event)
;;     ;; (notify-event ev-loop)
;;     ))




(defmacro curry (function &rest args)
  "Curry a function with the given arguments."
  `(lambda (&rest more-args)
     (apply ,function (append ',args more-args))))

;; (defmacro with-event-loop ((&key (id nil) (fps nil) (async t) (until t)) &body body)
;;   "Execute the body in the event loop with the given id.
;; If no id is given, a new event loop is created. If async is
;; true, the event loop is started in a new thread. If until is
;; true, the event loop is executed until the body returns. If until
;; is a callable, the event loop is executed until the callable
;; returns nil. The event loop is returned."
;;   (let ((ev-loop (gensym)))
;;     `(let ((,ev-loop (if ,id
;;                        (get-event-loop ,id)
;;                        (make-event-loop))))
;;        (with-slots (fps loop-fun async until) ,ev-loop
;;          (setf loop-fun (lambda () (progn ,@body)))
;;          (setf async ,async)
;;          (setf fps ,fps)
;;          (setf until ,until))
;;        (start-event-loop ,ev-loop)
;;        ,ev-loop)))




;; (defun trigger-error-event (event target)
;;   (tri event target :priority *error-event-priority*))

(defun error-event-handler (event target)
  ;; Define custom error handling logic here
  (format t "An error occurred in the event '~A' with target '~A'." event target))

;; (add-event-listener :error t #'error-event-handler)

;; (defvar *event-lock* (bt:make-lock "event lock")) ;; Use reader-writer lock
;; (defun process-loop-events ()
;;   "Process the events in the queue."
;;   (let ((event-loop-hash (gethash id *event-loops*))) ;; Cache lookup
;;     (loop
;;       (let ((event (bt:with-recursive-lock-held (*event-lock*)
;;                      (dequeue *event-queue*))))
;;         (when event
;;           (trigger-event event (event-target event) (event-data event)))))))

;; (defmacro defloop (name &key (loop-fun nil loop-fun-p)
;;                     (fps nil fps-p)
;;                     (async t async-p)
;;                     (until t until-p))
;;   "Define an event loop with a name, optional loop function, fps, async flag and until flag."
;;   (let ((loop-sym (intern (format nil "~A-LOOP" name))))
;;     `(progn
;;        (defparameter ,loop-sym (clevelib.event-loops:make-event-loop))
;;        (defmethod ,name ((tui tui))
;;          (setf (clevelib.event-loops:loop-fun ,loop-sym)
;;            ,(if loop-fun-p loop-fun '(lambda () nil)))
;;          (setf (clevelib.event-loops:fps ,loop-sym) ,fps)
;;          (setf (clevelib.event-loops:async ,loop-sym) ,async)
;;          (setf (clevelib.event-loops:until ,loop-sym) ,until)
;;          (when ,fps-p
;;            (setf (clevelib.event-loops:loop-fun ,loop-sym)
;;              (clevelib.event-loops:curry
;;                (clevelib.event-loops:make-fps-loop ,fps)
;;                (clevelib.event-loops:loop-fun ,loop-sym))))
;;          (when ,async-p
;;            (setf (clevelib.event-loops:loop-fun ,loop-sym)
;;              (clevelib.event-loops:curry
;;                (clevelib.event-loops:make-async-loop)
;;                (clevelib.event-loops:loop-fun ,loop-sym))))
;;          (when ,until-p
;;            (setf (clevelib.event-loops:loop-fun ,loop-sym)
;;              (clevelib.event-loops:curry
;;                (clevelib.event-loops:make-until-loop ,until)
;;                (clevelib.event-loops:loop-fun ,loop-sym))))
;;          (clevelib.event-loops:start-event-loop ,loop-sym)
;;          ,loop-sym))))

(defmacro defloop (name event-arg (&key
                                    (run t )
                                    (input-queue nil )
                                    (output-queue nil )
                                    (wait t )
                                    (fps nil )
                                    (mgr nil ))
                    &body body)
  "Define an event loop with a name, optional loop function, fps, async flag and until flag."
  (let ((thread-sym (intern (string-upcase (format nil "~A-LOOP" name))))
         (run-meth (intern (string-upcase (format nil "~A-run" name))))
         (stop-meth (intern (string-upcase (format nil "~A-stop" name))))
         (out-queue-sym (intern (string-upcase(format nil "~A-out-queue" name))))
         (in-queue-sym (intern (string-upcase(format nil "~A-in-queue" name))))
         (state-sym (intern (string-upcase(format nil "~A-state" name))))
         (rendered-fun `(lambda (,event-arg) ,@body)))
    `(progn
       (let ((inst (make-instance 'evloop :wait ,wait :fps ,fps :loop-fun ,rendered-fun)))
         ;; (defparameter ,thread-sym (clevelib.event-loops:make-event-loop))
         (defgeneric ,run-meth (sys))
         (defmethod ,run-meth ((sys clevelib.event-system:event-system))
           ;; (format t "dud inst:~a sys:~a" inst sys)
           (clevelib.event-system:idempotent-add-loops sys inst)
           (setf  (thread inst)
             (start-thread (clevelib.event-system:event-system-pool sys)
               (lambda ()(run inst))
               :description ,(format nil "~A-LOOP" name))))
         (defgeneric ,stop-meth (sys))
         (defmethod ,stop-meth((sys clevelib.event-system:event-system)))
         (stop inst)
         (defmethod ,out-queue-sym((sys clevelib.event-system:event-system))
           (o-queue inst))
         (defmethod ,in-queue-sym((sys clevelib.event-system:event-system))
           (i-queue inst))
         (defmethod ,thread-sym((sys clevelib.event-system:event-system))
           inst)
         (defmethod ,state-sym((sys clevelib.event-system:event-system))
           (state inst))
         ;; (format t "~a" inst)
         ;; TODO maybe typecheck queues?
         (when ,input-queue
           (setf (i-queue inst) ,input-queue))
         (when ,output-queue
           (setf (o-queue inst) ,output-queue))
         ;; (output-queue ,thread-sym) ,output-queue)))
         `(declare (ignorable run-meth thread-sym stop-meth out-queue-sym in-queue-sym state-sym))
         (when ,(and run (not (null mgr)) `(,run-meth ,mgr)))))))

;; (let (eve (make-instance 'clevelib.event-system:event-system)
;;          (x 0))
;;     (defloop no-1 evevevent ()
;;       (setf x evevevent)
;;       (format nil "no2 ~a" evevevent))
;;     (no-1-run (make-instance 'clevelib.event-system:event-system))



;;     )


;; (defloop axx swob ()
;;   (format t "swob ~a" swob))
;; (no-1-run (make-instance 'clevelib.event-system:event-system))
;; (axx-in-queue)
;; (axx-run (make-instance 'clevelib.event-system::event-system))
;; (defmacro dudu (name) (intern (symbol-name name) :keyword))
;; (defmacro defthread (name &key (loop-fun nil loop-fun-p)
;;                            (input-queue nil input-queue-p)
;;                            (output-queue nil output-queue-p)
;;                            async)
;;   "Define a thread with a name, optional loop function, input queue, output queue and async flag."
;;   (let ((thread-sym (intern (format nil "~A-THREAD" name))))
;;     `(progn
;;        (defparameter ,thread-sym (clevelib.event-loops:make-event-loop))
;;        (defmethod ,name ((tui tui))
;;          (setf (clevelib.event-loops:loop-fun ,thread-sym)
;;                ,(if loop-fun-p loop-fun '(lambda () nil)))
;;          (setf (clevelib.event-loops:async ,thread-sym) ,async)
;;          (when ,input-queue-p
;;            (setf (clevelib.event-loops:input-queue ,thread-sym) ,input-queue))
;;          (when ,output-queue-p
;;            (if (boundp ',output-queue)
;;                (setf (clevelib.event-loops:output-queue ,thread-sym) ,output-queue)
;;                (setf ,output-queue (clevelib.event-loops:make-queue)
;;                      (clevelib.event-loops:output-queue ,thread-sym) ,output-queue)))
;;          (clevelib.event-loops:start-event-loop ,thread-sym)))))
