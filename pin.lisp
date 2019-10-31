(in-package :e/pin)

(defclass pin ()
  ((symbol :accessor symbol :initarg :symbol)))

(defmethod make-pin (sym)
  (make-instance 'pin :symbol sym))

(defmethod pin-equal ((self pin) (other pin))
  (eq (symbol self) (symbol other)))

(defmethod as-symbol ((self pin))
  (symbol self))
