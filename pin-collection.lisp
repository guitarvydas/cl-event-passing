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

(defmethod as-list ((pb pin-collection))
  "return a list of pins"
  (let ((list nil))
    (maphash #'(lambda (sym pin)
                 (declare (ignore sym))
                 (push pin list))
             (pin-collection pb))
    list))

(defmethod ensure-member ((pb pin-collection) (pin-sym symbol))
  (multiple-value-bind (pin success)
      (gethash pin-sym (pin-collection pb))
    (assert success)
    pin))

(defmethod ensure-member ((pb pin-collection) (pin e/pin:pin))
  (mapc #'(lambda (other)
            (when (e/pin:pin-equal pin other)
              (return-from ensure-member pin)))
        (as-list pb))
  (assert nil))

(defmethod lookup-pin ((pb pin-collection) (pin e/pin:pin))
  (ensure-member pb pin)) ;; same code as above

(defmethod lookup-pin ((pb pin-collection) (pin-sym symbol))
  (ensure-member pb (e/pin:make-pin pin-sym))) ;; same code as above
