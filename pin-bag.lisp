(in-package :e/pin-bag)

;; in this implementation, we define a pin-bag as a hash table indexed by e/pin:pin's

(defclass pin-bag ()
  ((pin-bag :accessor pin-bag :initform (make-hash-table :test 'equal))))

(defmethod from-list ((list-of-syms CONS))
  (let ((bag (make-instance 'pin-bag)))
    (mapc #'(lambda (sym)
              (setf (gethash sym (pin-bag bag))
                    (e/pin:make-pin sym)))
          list-of-syms)
    bag))

(defun make-empty-bag ()
  (let ((bag (make-instance 'pin-bag)))
    bag))

(defmethod as-list ((pb pin-bag))
  "return a list of pins"
  (let ((list nil))
    (maphash #'(lambda (sym pin)
                 (declare (ignore sym))
                 (push pin list))
             (pin-bag pb))
    list))

(defmethod ensure-member ((pb pin-bag) (pin-sym symbol))
  (multiple-value-bind (pin success)
      (gethash pin-sym (pin-bag pb))
    (assert success)
    pin))

(defmethod ensure-member ((pb pin-bag) (pin e/pin:pin))
  (mapc #'(lambda (other)
            (when (e/pin:pin-equal pin other)
              (return-from ensure-member pin)))
        (as-list pb))
  (assert nil))

(defmethod lookup-pin ((pb pin-bag) (pin e/pin:pin))
  (ensure-member pb pin)) ;; same code as above

(defmethod lookup-pin ((pb pin-bag) (pin-sym symbol))
  (ensure-member pb (e/pin:make-pin pin-sym))) ;; same code as above
