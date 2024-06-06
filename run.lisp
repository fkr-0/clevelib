"
Usage:

rlwrap sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
calls the main function.

Then, we are given the lisp prompt.

If you don't want to land in the REPL, you can (quit) below or call lisp with the --non-interactive flag.

Another solution to run the app is to build and run a binary (see README).
"

(load "clevelib.asd")
(asdf:load-system "clevelib")

; (ql:quickload "clevelib")
; (ql:quickload "cl-repl")
(in-package :clevelib)
(handler-case
  (progn
    (let ((x 10))
      ;; (clevelib.event-loops:with-event-loop (:fps 1 :until (lambda () (not (zerop x))) )
      ;;   (format t "~A.~%" x)
      ;;   (setf x (- x 1)))
      (cl-repl:main)))
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (uiop:quit 1)))
