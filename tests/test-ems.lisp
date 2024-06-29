;; (in-package :clevelib-tests)
(defpackage :ems-tests
  (:use :cl :fiveam :log4cl)
  (:import-from :clevelib.api
    :relay-message
    #:ems-root
    :on-message)
  (:import-from :clevelib.message
    :illegal-state-condition
    :message
    :message-state
    :message-data
    :reenqueue-handle
    :async-handle
    :event-message)
  (:import-from :clevelib.ems
    #:ems
    #:messenger
    #:enqueue-message
    #:add-async-exec-handler
    #:add-response-handler
    #:start
    #:running-p
    #:stop
    #:ems-message-loop
    #:ems-pool
    #:messenger-name
    #:messenger-parent
    #:entity-handlers
    #:messenger-children))

(in-package :ems-tests)

(defmethod on-message ((m messenger) (message message) &key (state nil) )
  (when (not (null state))
    (setf (message-state message) state))
  ;; check if message-data is a list
  (if (listp (message-data message))
    (setf (message-data message) (append (message-data message) (list (messenger-name m))))
    (setf (message-data message) (append (list (message-data message))(list (messenger-name m)))))
  (log-info "~A> Receiving Message: ~% ~A (~A) ~A~%" m message state (message-data message)))
(defun test-propagation (ems trgt expected-data &key (bubbling-p t) (capturing-p t))
  (let* ((msg (make-instance 'message :bubbles-p bubbling-p :captures-p capturing-p
                :message-type :custom-message :target trgt)))
    (relay-message ems msg)
    (print (format nil "~A : ~A " (equal (message-data msg)
                                    (mapcar #'symbol-name
                                      expected-data))
             (message-data msg)))
    (fiveam:is (equal (mapcar #'string-upcase (message-data msg))

                 (mapcar #'symbol-name
                   expected-data)))))



(defun test-broadcast (ems target expected-data &key (bubbling-p nil) (capturing-p nil))
  (let* ((msg (make-instance 'message :message-type :custom-message :target target
                :bubbles-p bubbling-p :captures-p capturing-p)))
    (if (or bubbling-p capturing-p)
      (fiveam:signals error "Broadcast message cannot bubble or capture."
        (relay-message ems msg))
      (progn (relay-message ems msg)
        (print (message-data msg))
        (dolist (item (message-data msg))
          (print item)
          (is (member (string-upcase item)
                (mapcar #'symbol-name
                  expected-data) :test #'string=)))
        (is (= (length (message-data msg)) (length expected-data)))))))


(def-suite ems-test-suite
  :description "Test suite for the clevelib.threads package")
;; :in testmain)

(format t "~&~%Running tests for clevelib.threads package~%~%")
(in-suite ems-test-suite)
;; (defpackage :ems-tests
;;   (:use :cl :fiveam)
;;   (:import-from :ems
;;                 #:ems
;;                 #:messenger
;;                 #:on-message
;;                 #:enqueue-message
;;                 #:add-async-exec-handler
;;                 #:add-response-handler
;;                 #:start
;;                 #:running-p
;;                 #:stop
;;                 #:messenger-name
;;                 #:messenger-parent
;;                 #:entity-handlers
;;                 #:messenger-children))


(test ems-initialization
  (let* ((ems (make-instance 'ems)))
    (is (null (ems-root ems)))
    (is (typep (ems-message-loop ems) 'clevelib.event-loop:event-loop))
    (is (typep (ems-pool ems) 'clevelib.thread-pool:thread-pool))))

(test messenger-initialization
  (let* ((messenger (make-instance 'messenger)))
    (is (stringp (messenger-name messenger)))
    (is (null (messenger-parent messenger)))
    (is (hash-table-p (entity-handlers messenger)))
    (is (null (messenger-children messenger)))))

(test enqueue-message
  (let* ((ems (make-instance 'ems))
          (message (make-instance 'event-message)))
    (enqueue-message ems message)
    (is (= 1 (clevelib.queue:queue-length
               (clevelib.event-loop:i-queue (ems-message-loop ems)))))))

(test add-async-exec-handler
  (let* ((ems (make-instance 'ems))
          (message (make-instance 'event-message)))
    (add-async-exec-handler ems message)
    (is (functionp (async-handle message)))))

(test add-response-handler
  (let* ((ems (make-instance 'ems))
          (message (make-instance 'event-message)))
    (add-response-handler ems message)
    (is (functionp (reenqueue-handle message)))))

;; TODO fix this
;; (test start-stop-ems
;;   (let* ((ems (make-instance 'ems)))
;;     (start ems)
;;     (is (running-p ems))
;;     (stop ems)
;;     (sleep 1)
;;     (is (not (running-p ems)))))

(test messenger-parent-child
  (let* ((parent (make-instance 'messenger))
          (child (make-instance 'messenger :parent parent)))
    (is (eq parent (messenger-parent child)))
    (is (member child (messenger-children parent)))))



(test on-message-with-state
  (let* ((messenger (make-instance 'messenger))
          (message (make-instance 'message :message-data '(1 2 3))))
    (fiveam:signals illegal-state-condition
      (on-message messenger message :state :some-state))

    (is (eq :new (message-state message)))))

(test on-message-with-list-data
  (let* ((messenger (make-instance 'messenger :name "test-messenger"))
          (message (make-instance 'message :message-data '(1 2 3))))
    (on-message messenger message)
    (is (equal '(1 2 3 "test-messenger") (message-data message)))))

(test on-message-with-non-list-data
  (let* ((messenger (make-instance 'messenger :name "test-messenger"))
          (message (make-instance 'message :message-data 42)))
    (on-message messenger message)
    (is (equal '(42 "test-messenger") (message-data message)))))

(test on-message-with-null-state
  (let* ((messenger (make-instance 'messenger))
          (message (make-instance 'message)))
    (on-message messenger message)
    (is (eq (message-state message) :new))))


;; (defvar *sample-ems* (make-instance 'ems))
;; (let* ((e (make-instance 'ems))
;;         (m (make-instance 'messenger :name "root"))
;;         (m1 (make-instance 'messenger :name "child1" :parent m))
;;         (m2 (make-instance 'messenger :name "child2" :parent m))
;;         (m3 (make-instance 'messenger :name "child3" :parent m1))
;;         (m4 (make-instance 'messenger :name "child4" :parent m3)))
;;   ;; (relay-message e (make-instance 'message :message-type "Hello World"
;;   ;;                    :target m4))
;;   (on-message e (make-instance 'message :message-type "Hello World"
;;                   :target m2 :bubbles-p nil :captures-p nil))
;;   (start e)
;;   )
;; (defun r-child (parent)
;;   "Append a handler to ENTITY that records the route of the message."

;;   (let ((entity (if parent (make-instance 'propagation-entity
;;                              :name (format nil "~a.~a" (entity-name parent)
;;                                      (length (entity-children parent))))
;;                   (make-instance 'propagation-entity :name "R"))))
;;     (when parent (add-child parent entity))
;;     (append-hash (entity-handlers entity)
;;       (lambda (e m)
;;         (setf (message-data m) (append (message-data m) (list (entity-name e))))
;;         (format t "~a>: ~a | MessageState: ~a~% "
;;           (length (message-data m))
;;           (entity-name e)
;;           (message-state m))
;;         (format t "Path: ~a~%" (message-data m)))
;;       :custom-message)
;;     entity))

(fiveam:test broadcast-message
  (let* ((m (make-instance 'messenger :name "root"))
          (e (make-instance 'ems :root m))
          (m1 (make-instance 'messenger :name "child1" :parent m))
          (m2 (make-instance 'messenger :name "child2" :parent m))
          (m3 (make-instance 'messenger :name "child3" :parent m1))
          (m4 (make-instance 'messenger :name "child4" :parent m3))
          (m6 (make-instance 'messenger :name "child6" :parent m2))
          (m5 (make-instance 'messenger :name "child5" :parent m4)))
    (test-broadcast e :broadcast '(root child1 child3 child4 child5 child6 child2) :bubbling-p nil :capturing-p nil)))

(fiveam:test bubble-capture-true
  (let* ((e (make-instance 'ems))
          (m (make-instance 'messenger :name "root"))
          (m1 (make-instance 'messenger :name "child1" :parent m))
          (m2 (make-instance 'messenger :name "child2" :parent m))
          (m3 (make-instance 'messenger :name "child3" :parent m1))
          (m4 (make-instance 'messenger :name "child4" :parent m3))
          (m5 (make-instance 'messenger :name "child5" :parent m4)))
    (declare (ignore m2))
    (test-propagation e m5 '(root child1 child3 child4 child5) :bubbling-p nil :capturing-p t)))

;; Test case: Capture enabled, Bubble disabled
(fiveam:test capture-true-bubble-false
  (let* ((e (make-instance 'ems))
          (m (make-instance 'messenger :name "root"))
          (m1 (make-instance 'messenger :name "child1" :parent m))
          (m2 (make-instance 'messenger :name "child2" :parent m))
          (m3 (make-instance 'messenger :name "child3" :parent m1))
          (m4 (make-instance 'messenger :name "child4" :parent m3))
          (m5 (make-instance 'messenger :name "child5" :parent m4)))
    (declare (ignore m2))
    (test-propagation e m5 (reverse '( child5 child4 child3 child1 root)) :bubbling-p nil :capturing-p t)))

;; Test case: Capture disabled, Bubble enabled
(fiveam:test capture-false-bubble-true
  (let* ((e (make-instance 'ems))
          (m (make-instance 'messenger :name "root"))
          (m1 (make-instance 'messenger :name "child1" :parent m))
          (m2 (make-instance 'messenger :name "child2" :parent m))
          (m3 (make-instance 'messenger :name "child3" :parent m1))
          (m4 (make-instance 'messenger :name "child4" :parent m3))
          (m5 (make-instance 'messenger :name "child5" :parent m4)))
    (declare (ignore m2))
    (test-propagation e m5 '(child5 child4 child3 child1 root) :bubbling-p t :capturing-p nil)))

;; Test case: Capture and Bubble disabled
(fiveam:test capture-false-bubble-false
  (let* ((e (make-instance 'ems))
          (m (make-instance 'messenger :name "root"))
          (m1 (make-instance 'messenger :name "child1" :parent m))
          (m2 (make-instance 'messenger :name "child2" :parent m))
          (m3 (make-instance 'messenger :name "child3" :parent m1))
          (m4 (make-instance 'messenger :name "child4" :parent m3))
          (m5 (make-instance 'messenger :name "child5" :parent m4)))
    (test-propagation e m5 '(child5) :bubbling-p nil :capturing-p nil)
    (test-propagation e m4 '(child4) :bubbling-p nil :capturing-p nil)
    (test-propagation e m '(root) :bubbling-p nil :capturing-p nil)
    (test-propagation e m '(root) :bubbling-p t :capturing-p nil)
    (test-propagation e m '(root) :bubbling-p t :capturing-p t)
    (test-propagation e m '(root) :bubbling-p nil :capturing-p t)
    ))
(test illegal-state-condition
  (let ((msg (make-instance 'message :message-type :custom-message)))
    (fiveam:signals illegal-state-condition
      (setf (message-state msg) :illegal-state))))
