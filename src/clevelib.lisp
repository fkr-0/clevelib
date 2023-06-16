(defpackage :clevelib
  (:use :cl :bt :log4cl :clevelib.core :clevelib.macros)
  (:export :main

    ))

(in-package :clevelib)

;; Define your project functionality here...

(defun help ()
  (format t "~&Usage:

  clevelib [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (format t "No Op. Exit"))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
