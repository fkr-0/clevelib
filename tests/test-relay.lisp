;; (in-package :clevelib-tests)
;; (def-suite relay-tests
;;   :description "Test suite for the clevelib.macros package"
;;   :in testmain)

;; (in-suite relay-tests)
;; ;; (use-package :clevelib)

;; (test message-handlers
;;   (let ((relay (make-instance 'relay)))
;;     (setf (gethash :info (sink-handlers relay))
;;       (make-hash-table :test 'equal))
;;     (setf (gethash "target1" (gethash :info
;;                                (sink-handlers relay)))
;;       (list #'handler1))
;;     (setf (gethash "target2" (gethash :info
;;                                (sink-handlers relay)))
;;       (list #'handler2))
;;     (let ((m (make-instance 'message :target "target1" :type 'info)))
;;       (is (equal (list #'handler1)
;;             (relay-get-handlers relay m))))
;;     (let ((m (make-instance 'message :target "target2" :type 'info)))
;;       (is (equal (list #'handler2)
;;             (relay-get-handlers relay m))))
;;     (let ((m (make-instance 'message :target nil :type 'info)))
;;       (is (equal '(#'handler1 #'handler2) (relay-get-handlers relay m))))))

;; (test no-handlers
;;   (let ((relay (make-instance 'relay)))
;;     (let ((m (make-instance 'message :target "target" :type 'info)))
;;       (is (null (relay-get-handlers relay m))))))

;; (test test-relay-message
;;   (let ((relay (make-instance 'relay))
;;          (message (make-instance 'message :message-type :test :target :target)))
;;     (is (relay-message relay message))))

;; (test test-relay-receive-handler
;;   (let ((relay (make-instance 'relay)))
;;     (is (funcall (relay-receive-handler relay) (make-instance 'message :message-type :test :target :target)))))

;; (test test-message-handlers
;;   (let ((relay (make-instance 'relay))
;;          (message (make-instance 'message :message-type :test :target :target)))
;;     (is (message-handlers relay message))))
