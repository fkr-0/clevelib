
;; ======================================================================
;; FiveAM Tests
;; ======================================================================

;; The following tests are written using the FiveAM testing framework.
;; The tests can be run using the following command:


(in-package :clevelib-tests
  )

(use-package :fiveam)
;; (use-package :clevelib.channel)
(def-suite channels
  :in testmain
  :description "Tests for channels"

  ;; :in-order-to ((test channel-1)
  ;;               (test channel-2)
  )

(in-suite channels)
;; FiveAm expect errors: https://common-lisp.net/project/fiveam/#Expecting-errors
;; E.g., (expect-error (foo 1 2 3) 'error-type)

;; FiveAm expect signal: https://common-lisp.net/project/fiveam/#Expecting-signals

(test channel-1
  (let ((channel (make-channel)))

    (gogo
      (loop for i from 1 to 2 do
        (-> channel i)))

    (sleep 0.3)
    (is (= 1 (<- channel)))
    (is (= 2 (<- channel)))
    (channel-close channel)
    (sleep 0.3)
    (signals simple-error (-> channel 5))
    (is (channel-closed channel))
    ))
(test channel-1-1
  (let ((channel (make-channel)))
    (signals simple-error
      (gogo (progn (sleep 0.3)
              (let ((i 0))
                (loop
                  (incf i)
                  (-> channel i)))))
      (is (= 1 (<- channel)))
      (is (= 2 (<- channel)))
      (sleep 0.3)
      (channel-close channel)
      (sleep 0.3)
      (-> channel 5) )
    (is (channel-closed channel))
    ))

(test channel-2
  (let ((res nil)
         (channel (make-channel)))
    (gogo (loop (let ((value (<- channel)))
                  (push value res ))))
    (sleep 0.3)
    (-> channel 4)
    (-> channel 3)
    (-> channel 2)
    (-> channel 1)
    (is (equal '(1 2 3 4) res ))))

(test channel-2-2
  (let ((res nil)
         (channel (make-channel)))
    (gogo (loop (progn (sleep 0.3)
                  (let ((value (<- channel)))
                    (push value res )))))
    (-> channel 4)
    (-> channel 3)
    (-> channel 2)
    (-> channel 1)
    (is (equal '(1 2 3 4) res ))))
;; (test channel-1
;;   (let ((channel (make-channel)))
;;     (gogo (loop (let ((value (<- channel)))
;;                   (print value))))
;;     (-> channel 1)
;;     (-> channel 2)
;;     (-> channel 3)
;;     (-> channel 4)
;;     (is (= 1 (<- channel)))
;;     (is (= 2 (<- channel)))
;;     (sleep 0.3)
;;     (channel-close channel)
;;     (sleep 0.3)
;;     (signals simple-type-error  (-> channel 5) )
;;     (signals simple-type-error  (<- channel ) )
;;     (is (channel-closed channel))
;;     ))
(test gogo-macro
  (let ((result nil))
    (gogo
      (setf result 42))
    (sleep 0.1)
    (is (= result 42))))

(test channel-send-receive
  (let ((channel (make-channel))
         (vv '(1 2 3))
         (values '(1 2 3)))
    (gogo (loop for v in vv do (progn (-> channel v) ;; (format t "write ~a~%" v)
                                 )))
    (is (= (length values) 3))
    (loop repeat 3 do
      (is (= (pop values) (<- channel))))
    (is (null values))))
(test channel-send-receive-2
  (let ((channel (make-channel))
         (lock (sb-thread:make-mutex))
         (values '(1 2 3)))
    (gogo (loop for v in values do (progn (-> channel v) ;; (format t "write ~a~%" v)
                                     )))
    (is (= (length values) 3))
    (loop repeat 3 do
      (is (= (<- channel)(pop values) )))
    (is (null values))))
(test unbuffered-channel
  (let ((c (make-channel )))
    (let ((result nil))
      (bt:make-thread
        (lambda ()
          (sleep 0.1)
          (-> c 42)))
      (setf result (<- c))
      (is (= result 42)))))

(test buffered-channel
  (let ((c (make-channel :capacity 2)))
    (-> c 1)
    (-> c 2)
    (is (= (<- c) 1))
    (is (= (<- c) 2))))

;; (test select-macro

;;   (let ((c1 (make-channel))
;;          (c2 (make-channel))
;;          (c3 (make-channel))
;;          (result nil))
;;     (gogo
;;       (loop
;;         (select
;;           ((<- c1) (setf result 1))
;;           ((<- c2) (setf result 2))
;;           ((<- c3) (setf result 3)))))
;;     (-> c2 42)
;;     (sleep 0.1)
;;     (is (= result 2))))


;; (test mutex-condition-variable
;;   (let ((mutex (sb-thread:make-mutex))
;;          (cvar (bt:make-condition-variable))
;;          (result nil))
;;     (bt:make-thread
;;       (lambda ()
;;         (sleep 0.1)
;;         (sb-thread:with-mutex (mutex)
;;           (setf result 42)
;;           (sb-thread:condition-notify cvar))))
;;     (sb-thread:with-mutex (mutex)
;;       (loop until result do (sb-thread:condition-wait cvar mutex)))
;;     (is (= result 42))))
