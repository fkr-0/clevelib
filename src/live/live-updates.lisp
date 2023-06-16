;;;; live-updates.lisp
;;;;
;;;; This module manages live updates of application state, including:
;;;; - Registering and unregistering live update functions
;;;; - Efficiently tracking and processing updates to application state
;;;; - Handling dependencies between state properties and update functions
;;;; - Managing and scaling update intervals based on application load

;; Example usage:

;; (defun example-update-function ()
;;   (format t "Updating application state...~%"))

;; (register-live-update #'example-update-function :interval 5)

;; (run-in-thread #'live-update-event-loop) ; Run live update event loop in a separate thread
(defpackage :clevelib.live-updates
  (:use :common-lisp)
  (:export #:register-live-update
           #:unregister-live-update
           #:live-update-should-run-p))

(in-package :clevelib.live-updates)

(defparameter *live-update-registry* (make-hash-table)
  "Hash table storing live update functions and their associated metadata.")

(defun register-live-update (update-function &key interval)
  "Register an UPDATE-FUNCTION to be called periodically with the specified INTERVAL."
  (setf (gethash update-function *live-update-registry*)
        (list :interval interval
              :last-update (get-universal-time))))

(defun unregister-live-update (update-function)
  "Unregister the specified UPDATE-FUNCTION from the live update registry."
  (remhash update-function *live-update-registry*))

(defun live-update-should-run-p (update-function)
  "Determine if the specified UPDATE-FUNCTION should run, based on its last update time and interval."
  (let* ((metadata (gethash update-function *live-update-registry*))
         (interval (getf metadata :interval))
         (last-update (getf metadata :last-update))
         (current-time (get-universal-time)))
    (>= (- current-time last-update) interval)))
(defun process-live-updates ()
  "Process live updates by running registered update functions when appropriate."
  (let ((update-functions (hash-table-keys *live-update-registry*))
        (metadata-cache (make-hash-table :test 'eq)))
    (dolist (update-function update-functions)  
      ;; Cache metadata lookup
      (setf (gethash update-function metadata-cache) 
            (gethash update-function *live-update-registry*))
    (loop
      (let* ((next-update (heap-pop *update-schedule*))  
             ;; Use min-heap to get next soonest update  
             (metadata (gethash next-update metadata-cache))
             (interval (getf metadata :interval))  
             (last-update (getf metadata :last-update))  
             (current-time (get-universal-time)))
        (when (>= (- current-time last-update) interval)  
          (funcall next-update)  
          (setf (getf metadata :last-update) current-time)  
          ;; Update heap with new next scheduled run  
          (heap-push *update-schedule* next-update interval))))))

(defun register-live-update (update-function interval)  
  "Register an UPDATE-FUNCTION to be called periodically with the specified INTERVAL."
  ;; Make interval a required argument
  (let ((metadata (list :interval interval  
                        :last-update (get-universal-time))))
    (setf (gethash update-function *live-update-registry*) metadata)
    ;; Push to min-heap
    (heap-push *update-schedule* update-function interval)))

(defun process-live-updates ()
  "Process live updates by running registered update functions when appropriate."
  (maphash (lambda (update-function metadata)
             (when (live-update-should-run-p update-function)
               (funcall update-function)
               (setf (getf metadata :last-update) (get-universal-time))))
           *live-update-registry*))

(defun live-update-event-loop ()
  "Main event loop for processing live updates."
  (loop
    (process-live-updates)
    (sleep 0.1))) ; Adjust sleep duration based on desired update frequency

