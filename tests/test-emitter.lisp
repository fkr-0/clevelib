(in-package :clevelib-tests)
(def-suite emitter-tests
  :description "Test suite for the clevelib.macros package"
  :in testmain)

(in-suite emitter-tests)
;; (use-package :clevelib)

;; (test clevelib::emit
;;   (let ((event (make-instance 'clevelib::event :name "test-event" :data '
;;                  (1 2 3))))
;;     (with-output-to-string
;;       (stream)
;;       (clevelib::emit event stream)
;;       (is
;;         (equal
;;           (get-output-stream-string stream)
;;           "{\"name\":\"test-event\",\"data\":[1,2,3]}")))))

;; (test clevelib::emit-empty-data
;;   (let ((event (make-instance 'clevelib::event :name "test-event")))
;;     (with-output-to-string
;;       (stream)
;;       (clevelib::emit event stream)
;;       (is (equal (get-output-stream-string stream)
;;             "{\"name\":\"test-event\"}")))))

;; (test clevelib::emit-nil-data
;;   (let ((event (make-instance 'clevelib::event :name "test-event" :data nil)))
;;     (with-output-to-string
;;       (stream)
;;       (clevelib::emit event stream)
;;       (is (equal (get-output-stream-string stream)
;;             "{\"name\":\"test-event\"}")))))
