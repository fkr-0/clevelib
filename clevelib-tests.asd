(in-package :asdf-user)
(defsystem "clevelib-tests"
  :description "Test suite for the clevelib system"
  :author "cbadger <cbadger@mail.com>"
  :version "0.0.1"
  :depends-on (:clevelib
                :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                 :serial t
                 :components ((:file "packages")
                               (:file "test-clevelib")
                               (:file "test-macros")
                               (:file "test-threads")
                               (:file "test-queues")))))

;; The following would not return the right exit code on error, but still 0.
;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
