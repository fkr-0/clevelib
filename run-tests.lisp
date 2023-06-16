
(load "clevelib.asd")
(load "clevelib-tests.asd")

(ql:quickload "clevelib-tests")

(in-package :clevelib-tests)

(uiop:quit (if (run-all-tests) 0 1))
