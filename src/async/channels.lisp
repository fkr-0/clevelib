(in-package :clevelib)

(defstruct channel
  (lock (make-lock) :type bt:lock)
  (queue (make-queue) :type list)
  (empty-condition (make-condition-variable) :type bt:condition-variable))

(defun make-channel ()
  (make-channel))

(defun channel-send (channel value)
  (bt:with-lock-held ((channel-lock channel))
    (enqueue value (channel-queue channel))
    (bt:condition-notify (channel-empty-condition channel))))

(defun channel-receive (channel)
  (bt:with-lock-held ((channel-lock channel))
    (loop while (null (channel-queue channel))
          do (bt:condition-wait (channel-empty-condition channel) (channel-lock channel)))
    (dequeue (channel-queue channel))))

(defun channel-close (channel)
  (bt:with-lock-held ((channel-lock channel))
    (bt:condition-notify-all (channel-empty-condition channel))))
