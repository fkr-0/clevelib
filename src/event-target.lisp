(in-package :clevelib.event-system)

;; (defclass hierarchical-event-target (event-target)
;;   ((children :initarg :children
;;      :accessor event-target-children
;;      :documentation "The children of the object."))
;;   (:documentation "An hierarchichal event target is a target
;; that is part of a hierarchy and therefore is principally able to support
;; functions such as delegation, bubbling and object that is assigned closures to
;; emit events, add and remove event handlers."))

(defmethod initialize-instance :after ((target event-target) &key (sink nil))
  (when sink (connect-target sink target)))


(defmethod set-up-handler ((target event-target) &key
                            event-type handler  (options :at-target) (priority nil))
  (add-handler target event-type
    (make-instance 'event-handler
      :event-type event-type
      :callback handler
      :priority priority
      :options options)))


;; (defmethod target-children ((target event-target))
;;   "Get the children of the target."
;;   (let ((children (event-target-children target)))
;;     (unless children
;;       (error "No children for target: ~a" target))
;;     (funcall children)))

;; (defmethod target-emit-event ((target event-target) event-type &optional ev-target data)
;;   "Emit an event of the given type on the target, passing the optional data to the event handler."
;;   (declare (type event-target target))
;;   (let ((emitter (event-target-emitter target)))
;;     (unless emitter
;;       (error "No emitter for target: ~a" target))
;;     (funcall emitter  event-type ev-target  data)))

