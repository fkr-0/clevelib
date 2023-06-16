(defpackage :clevelib.state
  (:use :cl)
  (:nicknames :cl.state)
  )

(in-package :clevelib.state)

(defvar *state* (make-hash-table))

(defstruct state
  values
  listeners)


(defun get-state-value (key)
  (gethash key *state*))

(defun (set-state-value (key value)
         (setf (gethash key *state*) value)))


