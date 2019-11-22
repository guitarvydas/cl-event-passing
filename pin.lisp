(in-package :e/pin)

(defclass pin ()
  ((pin-name :accessor pin-name :initarg :pin-name)
   (direction :accessor direction :initarg :direction)
   (pin-parent :accessor pin-parent :initarg :pin-parent)))
  

(defmethod input-p ((self pin))
  (eq :input (direction self)))

(defmethod output-p ((self pin))
  (eq :output (direction self)))

(defun new-pin (&key (pin-name "") (direction :in) pin-parent) ;; parent unbound by default - always an error if this happens
  (make-instance 'pin :pin-name pin-name :direction direction :pin-parent pin-parent))

(defmethod pin-equal ((self pin) (other pin))
  (or (eq self other)
      (and
       (or (and (symbolp (pin-name self)) (equal (pin-name self) (pin-name other)))
           (and (stringp (pin-name self)) (string= (pin-name self) (pin-name other))))
       (eq (direction self) (direction other))
       (eq (pin-parent self) (pin-parent other)))))

(defmethod ensure-sanity (schem (self pin))
  (e/schematic::ensure-sanity schem (pin-parent self)))

(defmethod get-part ((self pin))
  (pin-parent self))