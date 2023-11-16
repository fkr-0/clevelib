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
