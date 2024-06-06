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
  :depends-on (log4cl bordeaux-threads cl-reexport)

  ;; Project stucture.
  :serial t
  :components ((:module "./src"
                 :serial t
                 :components (;; (:file "hierarchy")
                               ;;  (:file "base-system")
                               ;;  (:file "core/state")
                               ;;  (:file "event-system")
                               ;;  (:file "event-target")
                               ;;  (:file "event-handler")
                               ;;  (:file "event")
                               ;;  ;; (:file "async/synchronization")
                               ;;  ;; (:file "listeners/handlers")
                               ;;  ;; (:file "core/dispatcher")
                               ;;  ;; (:file "listeners/listeners")
                               ;;  (:file "core/event")
                               (:file "config")
                               (:file "queue")
                               (:file "thread")
                               (:file "async")
                               (:file "thread-pool")
                               (:file "event-loop")
                               (:file "channel")
                               ;; (:file "async/async")
                               ;;  (:file "utilities/macros")
                               (:file "message")
                               (:file "event")
                               (:file "emitter")
                               (:file "target")
                               (:file "translator")
                               (:file "relay")
                               (:file "clevelib"))))
  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "clevelib"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "clevelib:main")
