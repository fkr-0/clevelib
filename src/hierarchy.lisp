(defpackage :clevelib.hierarchy
  (:use :cl)
  (:export :hierarchy
    :add-object
    :objects
    :root
    :path
    :make-hierarchy))

(in-package :clevelib.hierarchy)

;;; A hierarchy is a directed acyclic graph of objects.  Each object
;;; has a parent object, except for the root object, which has no
;;; parent.  The path from the root to an object is the sequence of
;;; objects that must be traversed to get from the root to the object.
;;;
;;; The hierarchy is implemented as a hash table mapping objects to
;;; their parents. We use it to represent the hierarchy of objects in
;;; some program. To be able to use them through our event system and
;;; to achieve interoperability with any other system we make no assumptions
;;; about the objects in the hierarchy. Thus, our generics/methods operate on
;;; t, the most general type. Therefore we allow to dispatch events to any
;;; object.

(defclass hierarchy ()
  ((objects
     :initform (make-hash-table)
     :initarg :objects
     :accessor objects
     :documentation "The objects in the hierarchy.")
    (root :initform nil
      :accessor root
      :documentation "The root object in the hierarchy.")))

(defun make-hierarchy ()
  "Make a new hierarchy."
  (make-instance 'hierarchy :objects (make-hash-table)))

(defmethod add-object ((hierarchy hierarchy) object parent)
  "Add an object to the hierarchy with the given parent."
  (declare (type hierarchy hierarchy))
  (setf (gethash object (objects hierarchy)) parent))

(defmethod path ((hierarchy hierarchy) object)
  "Get the path from the root to the given object in the hierarchy."
  (if object
    (cons object (path hierarchy (gethash object (objects hierarchy))))
    '()))

(defun hash-table-keys (hash-table)
  "Return a list of all keys in the hash table."
  (let ((keys '()))
    (maphash (lambda (key value) (declare (ignore value))
               (push key keys)) hash-table)
    keys))


(defclass dumb-hierarchy ()
  ((objects :initform (make-hash-table)
     :accessor objects
     :documentation "The objects in the hierarchy.")))

(defmethod add-object ((hierarchy dumb-hierarchy) object connection)
  "Add an object to the hierarchy."
  (declare (ignore connection)
    (type dumb-hierarchy hierarchy))
  (setf (gethash object (objects hierarchy)) t))

(defmethod path ((hierarchy dumb-hierarchy) object)
  "Get a random path to the given object in the hierarchy."
  (if object
    (cons object (path hierarchy (random-object hierarchy)))
    '()))

(defmethod random-object ((hierarchy dumb-hierarchy))
  "Get a random object from the hierarchy."
  (let ((objects (hash-table-keys (objects hierarchy))))
    (when objects
      (nth (random (length objects)) objects))))
