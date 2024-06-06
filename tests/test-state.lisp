(in-package :clevelib-tests)
(def-suite state-test-suite
  :description "Test suite for the clevelib.macros package"
  :in testmain
  )

(in-suite state-test-suite)

(test test-transition-state ()
  (let ((*state* (make-state)))
    (transition-state :first-state)
    (is (equal (state-current-state *state*) :first-state))
    (is (equal (state-previous-state *state*) nil))
    (is (equal (state-state-history *state*) '((:first-state . nil))))
    (transition-state :second-state)
    (is (equal (state-current-state *state*) :second-state))
    (is (equal (state-previous-state *state*) :first-state))
    (is (equal (state-state-history *state*) '((:first-state . nil)
                                                (:second-state . :first-state))))))

(test test-revert-state ()
  (let ((*state* (make-state)))
    (transition-state :first-state)
    (revert-state)
    (is (equal (state-current-state *state*) nil))
    (is (equal (state-previous-state *state*) nil))
    (transition-state :first-state)
    (transition-state :second-state)
    (revert-state)
    (is (equal (state-current-state *state*) :first-state))
    (is (equal (state-previous-state *state*) nil))
    (revert-state)
    (is (equal (state-current-state *state*) nil))
    (is (equal (state-previous-state *state*) nil))))
(test symbol-to-string-conversion
  (is (equal (uncharming-tui.state::symbol-name-if-symbol 'test-symbol) "TEST-SYMBOL"))
  (is (equal (uncharming-tui.state::symbol-name-if-symbol "test-string") "test-string")))
(test transaction-macro-basic
  (let ((*application-state* (make-mock-state)))
    (with-transactional-access
      (setf (get-state-value *application-state* "test.path") "new value"))
    (is (equal (get-mock-state-value *application-state* "test.path") "new value"))))

(test transaction-macro-rollback
  (let ((*application-state* (make-mock-state)))
    (handler-case
      (with-transactional-access
        (setf (get-state-value *application-state* "test.path") "new value")
        (error "Force rollback"))
      (error (e)
        (declare (ignore e))))
    (is (null (get-mock-state-value *application-state* "test.path")))))
(test full-circle-state-management
  (let ((*application-state* (new 'application-state))
         (test-observer-notified nil))

    ;; Mock observer callback function
    (defun mock-observer-callback (old-state new-state)
      (declare (ignore old-state new-state))
      (setf test-observer-notified t))

    ;; Subscribe an observer
    (subscribe-to-state *application-state* "test.path" #'mock-observer-callback)

    ;; Perform a transactional update
    (with-transactional-access
      (setf (get-state-value *application-state* "test.path") "new value"))

    ;; Check if state is updated
    (is (equal (get-state-value *application-state* "test.path") "new value"))

    ;; Check if observer was notified
    (is test-observer-notified)))

(ql:quickload :fiveam)
;; (fiveam:run! 'uncharming-tui-tests)
