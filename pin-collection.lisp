(in-package :e/pin-collection)

;; a pin-collection is a collection of pins

;; each part has two pin collections - a collection of INPUT pins and a collection of OUTPUT pins

;; in this implementation, we define a pin-collection as a hash table indexed by e/pin:pin's

(defclass pin-collection ()
  ((pin-collection :accessor pin-collection :initform (make-hash-table :test 'equal))))

(defmethod from-list ((list-of-syms CONS))
  (let ((collection (make-instance 'pin-collection)))
    (mapc #'(lambda (sym)
              (setf (gethash sym (pin-collection collection))
                    (e/pin:make-pin sym)))
          list-of-syms)
    collection))

(defun make-empty-collection ()
  (let ((collection (make-instance 'pin-collection)))
    collection))

(defmethod as-list ((pc pin-collection))
  "return a list of pins"
  (let ((list nil))
    (maphash #'(lambda (sym pin)
                 (declare (ignore sym))
                 (push pin list))
             (pin-collection pc))
    list))

(defmethod ensure-member ((pc pin-collection) (pin-sym symbol))
  (multiple-value-bind (pin success)
      (gethash pin-sym (pin-collection pc))
    (assert success)
    pin))

(defmethod ensure-member ((pc pin-collection) (pin e/pin:pin))
  (mapc #'(lambda (other)
            (when (e/pin:pin-equal pin other)
              (return-from ensure-member pin)))
        (as-list pc))
  (assert nil))

(defmethod lookup-pin ((pc pin-collection) (pin e/pin:pin))
  (ensure-member pc pin)) ;; same code as above

(defmethod lookup-pin ((pc pin-collection) (pin-sym symbol))
  (ensure-member pc (e/pin:make-pin pin-sym))) ;; same code as above
