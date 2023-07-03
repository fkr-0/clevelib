(in-package :asdf-user)

(defsystem "clevelib"
  :author "cbadger <cbadger@mail.com>"
  :version "0.0.1"
  :license "MIT"
  :description "an event management system that should power a ui but got out of control"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (log4cl bordeaux-threads)

  ;; Project stucture.
  :serial t
  :components ((:module "./src"
                 :serial t
                 :components ((:file "core/state")
                               (:file "async/threads")
                               ;; (:file "async/synchronization")
                               ;; (:file "listeners/handlers")
                               ;; (:file "core/dispatcher")
                               ;; (:file "listeners/listeners")
                               (:file "core/event")
                               (:file "async/queue")
                               (:file "async/event-loops")
                               (:file "async/async")
                               (:file "utilities/macros")
                               ;; (:file "queues")
                               ;; (:file "clevelib")
                               (:file "./clevelib")
                               )))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "clevelib"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "clevelib:main")
