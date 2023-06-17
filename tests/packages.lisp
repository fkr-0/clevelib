;; (ql:quickload :fiveam)
(in-package :asdf-user)
(defpackage :clevelib-tests
  (:use :common-lisp
    :fiveam
    :clevelib
    :clevelib.core
    :clevelib.queues
    :clevelib.event-loops
    :clevelib.async
    :clevelib.macros
    ))


(in-package :clevelib-tests)
