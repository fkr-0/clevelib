;; (ql:quickload :fiveam)
(in-package :asdf-user)
(uiop:define-package :clevelib-tests
  (:use :common-lisp
    :fiveam
    :clevelib
    ;; :clevelib.event-system
    ;; :clevelib.channel
    ;; :clevelib.core
    ;; :clevelib.queue
    ;; :clevelib.thread
    ;; :clevelib.event-loops
    ;; :clevelib.async
    ;; :clevelib.macros
    ))


(in-package :clevelib-tests)
;; (asdf:test-system 'clevelib-tests)
