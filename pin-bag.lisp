(in-package :e/pin-bag)

;; in this implementation, we define a pin-bag as a hash table indexed by strings

(defclass pin-bag ()
  ((pin-bag :initform (make-hash-table :test 'equal))))

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
             pb)
    list))

(defmethod ensure-member ((pin e/pin:pin) (pb pin-bag))
  (some #'(lambda (other)
            (e/pin:pin-equal pin other))
        (as-list pb)))


