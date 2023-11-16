;;;; Event Pattern DSL Documentation
;;
;; This DSL provides a concise way to define and match event patterns in an event-driven system.
;;
;; Components of a Pattern:
;;
;; 1. Sequence: A specific order of events.
;;    Example: (sequence A B C)
;;
;; 2. Alternation: One of several events.
;;    Example: (either A B)
;;
;; 3. Repetition: An event occurring a specific number of times.
;;    Example: (repeat A 3)
;;
;; 4. Absence: An event not occurring within a specified time frame.
;;    Example: (not A :within 5s)
;;
;; 5. Time Constraint: A sequence of events occurring within a specific time frame.
;;    Example: (within (A B) 10s)
;;
;; 6. Wildcard: Any event.
;;    Example: (wildcard)
;;
;; Usage Example:
;;
;; Suppose we want to detect a pattern where event A is followed by either event B or C, but not if event D has occurred in the meantime.
;; This can be represented as:
;;
;; (sequence A (either B C) (not D))
;;
;; When this pattern is matched against a stream of events, it will return true if the specified sequence is detected.
;;
;;; NOTE: The current implementation includes basic patterns like sequences, alternations, repetitions, and absences. 
;; It also supports time constraints and wildcards. Further enhancements can be made to support more complex patterns and combinations.

;;; EXAMPLE:
;;; (defvar complex-pattern
;;;   (sequence (either 'eventA 'eventB) (not 'eventC) (repeat 'eventD 3) (within 5000 (sequence 'eventE 'eventF))))
;;; This pattern matches a sequence where either 'eventA' or 'eventB' occurs, followed by the absence of 'eventC', then three occurrences of 'eventD', and finally 'eventE' followed by 'eventF' within 5000 milliseconds.
;;; COMPARATOR KEYWORD:
;;; The DSL supports an optional :comparator keyword for custom event comparison. This allows for more flexible event matching, especially when events are complex objects or structures.
;;; For instance, if events are represented as instances of classes, you might want to match based on an event's type, its properties, or a combination of both.
;;; EXAMPLE:
;;; (defvar my-pattern (sequence '(event1) :comparator #'my-comparator-function))
;;; In this example, my-comparator-function is a custom comparison function that will be used when matching events against my-pattern.


(in-package :clevelib)


;; Event Pattern DSL

;; Define a pattern structure
(defstruct pattern
  sequence
  alternation
  repetition
  absence
  time-constraint
  wildcard
  comparator)
;; Define a macro for sequence pattern

(defmacro sequence (&rest events &key comparator)
    "Creates a sequence pattern from the given EVENTS.
    COMPARATOR is an optional function for custom event comparison."
    `(make-pattern :sequence ',events)) 

;; Define a macro for alternation pattern
(defmacro either (&rest events &key comparator)
    "Creates an alternation pattern from the given EVENTS.
    COMPARATOR is an optional function for custom event comparison."
    `(make-pattern :alternation ',events :comparator ,comparator))

;; Define a macro for repetition pattern
(defmacro repeat (event count &key comparator)
    "Creates a repetition pattern repeating EVENT COUNT times.
    EVENT can be any pattern. COMPARATOR is an optional function for 
    custom event comparison."
    `(make-pattern :repetition (list ',event ,count :comparator ,comparator)))

;; Define a macro for absence pattern
(defmacro not (event &key within comparator)
    "Creates an absence pattern matching if EVENT does not occur.
    WITHIN optionally constrains the scope that EVENT must be absent from."
    `(make-pattern :absence (list ',event ,within :comparator ,comparator)))

;; Define a macro for time constraint pattern
(defmacro within (events time &key comparator)
    "Creates a time constraint pattern where EVENTS must occur within TIME.
    COMPARATOR is an optional function for custom event comparison."
    `(make-pattern :time-constraint (list ',events ,time) :comparator ,comparator))

;; Define a macro for wildcard pattern
(defmacro wildcard (&key comparator)
    "Creates a wildcard pattern that matches any event.
    COMPARATOR is an optional function for custom event comparison."
    `(make-pattern :wildcard t :comparator ,comparator))

(defun match-event (event pattern &optional comparator)
  (if comparator
      (funcall comparator event pattern)
      (equal event pattern)))

(defun match-sequence (pattern events &optional comparator)
  "Match a sequence pattern against a list of events."
  (let ((sequence (pattern-sequence pattern)))
    (and sequence
         (every #'(lambda (e p) (match-event e p comparator))
                events
                sequence))))

(defun match-alternation (pattern events &optional comparator)
  "Match an alternation pattern against a list of events."
  (let ((alternatives (pattern-alternation pattern)))
    (some (lambda (alt) (match-event (first events) alt comparator)) 
    alternatives)))

(defun match-repetition (pattern events &optional comparator)
  "Match a repetition pattern against a list of events."
  (let* ((event (pattern-repetition pattern))
         (min-count (second pattern))
         (max-count (second pattern))
         (actual-count (count-if #'(lambda (e) (match-event e event comparator))
                                events)))
    (and (>= actual-count min-count)
         (<= actual-count max-count))))

(defun match-absence (pattern events &optional comparator)
  "Match an absence pattern against a list of events."
  (let ((event (pattern-absence pattern)))
    (not (some #'(lambda (e) (match-event e event comparator))
               events))))

(defun match-time-constraint (pattern events &optional comparator)
  "Match a time constraint pattern against a list of events.
  The time constraint is specified as a list of events and a time limit.
  The events must occur within the time limit."
  (let ((sequence (pattern-sequence pattern))
        (time-limit (pattern-time-constraint pattern)))
    ;; Assuming events are timestamped and stored as (timestamp . event) pairs
    (let ((start-time (car (first events)))
          (end-time (car (last events))))
      (and (match-sequence sequence events comparator)
           (<= (- end-time start-time) time-limit)))))

(defun match-wildcard (pattern events)
  "Match a wildcard pattern against a list of events."
  ;; Since wildcard matches any event, simply return true if there's an event
  (not (null events)))

(defun evaluate-patterns (patterns events)
  "Evaluate multiple patterns and return the first matching pattern (or all matching patterns)."
  (loop for pattern in patterns
        if (match-pattern pattern events)
        collect pattern))

(defun combine-patterns (&rest patterns)
  "Combine multiple patterns into a single pattern."
  (let ((combined-pattern (make-pattern)))
    (setf (pattern-sequence combined-pattern) (mapcan #'pattern-sequence patterns))
    combined-pattern))


(defun match-pattern (pattern events)
  "General pattern matching function that delegates to specific matchers based on the pattern type."
  (cond
    ((pattern-sequence pattern) (match-sequence pattern events))
    ((pattern-alternation pattern) (match-alternation pattern events))
    ((pattern-repetition pattern) (match-repetition pattern events))
    ((pattern-absence pattern) (match-absence pattern events))
    ((pattern-time-constraint pattern) (match-time-constraint pattern events))
    ((pattern-wildcard pattern) (match-wildcard pattern events))
    (t nil)))



;; Tests

;; FiveAM integration tests

(in-package #:fiveam)

(def-suite event-patterns-tests)

(in-suite event-patterns-tests)

(test sequence-pattern-test
  "Test for sequence pattern matching."
  (let ((pattern (sequence 'event1 'event2))
        (events '(event1 event2 event3)))
    (is (match-pattern pattern events))))

(test alternation-pattern-test
  "Test for alternation pattern matching."
  (let ((pattern (either 'event1 'event2))
        (events '(event2 event3)))
    (is (match-pattern pattern events))))

(test repetition-pattern-test
  "Test for repetition pattern matching."
  (let ((pattern (repeat 'event1 2))
        (events '(event1 event1 event2)))
    (is (match-pattern pattern events))))

(test absence-pattern-test
  "Test for absence pattern matching."
  (let ((pattern (not 'event1))
        (events '(event2 event3)))
    (is (match-pattern pattern events))))

(test wildcard-pattern-test
  "Test for wildcard pattern matching."
  (let ((pattern (wildcard))
        (events '(event1)))
    (is (match-pattern pattern events))))


(test combined-patterns-test
  "Test for combined patterns."
  (let ((pattern1 (sequence 'event1 'event2))
        (pattern2 (either 'event3 'event4))
        (pattern3 (repeat 'event5 2))
        (combined-pattern (combine-patterns pattern1 pattern2 pattern3))
        (events '(event1 event2 event3 event5 event5)))
    (is (match-pattern combined-pattern events))))

(test delicate-pattern-test
  "Test for more delicate patterns."
  (let ((pattern (sequence (either 'event1 'event2) (not 'event3) (repeat 'event4 3)))
        (events '(event1 event4 event4 event4)))
    (is (match-pattern pattern events))))

(test comparator-pattern-test
  "Test for custom comparator functionality."
  (let* ((custom-comparator (lambda (e p) (and (listp e) (equal (first e) p))))
         (pattern (sequence '(event1) :comparator custom-comparator))
         (events '(((event1) . data) ((event2) . data2))))
    (is (match-pattern pattern events))))

