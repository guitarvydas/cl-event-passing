(in-package :e/pin)

(defclass pin ()
  ((pin-name :accessor pin-name :initarg :pin-name)
   (pin-parent :accessor pin-parent :initarg :pin-parent)))
  
(defclass input-pin (pin) ())

(defclass output-pin (pin) ())

(defmethod input-p ((self pin))
  (eq 'input-pin (type-of self)))

(defmethod output-p ((self pin))
  (eq 'output-pin (type-of self)))

(defun new-pin (&key (pin-name "") (direction :in) pin-parent) ;; parent is unbound by default - always an error if parent is not bound (later)
  (make-instance
   (if (eq :input direction)
       'input-pin
     'output-pin)
   :pin-name pin-name
   :pin-parent pin-parent))

(defmethod pin-equal ((self pin) (other pin))
  (or (eq self other)
      (and
       (or (and (symbolp (pin-name self)) (equal (pin-name self) (pin-name other)))
           (and (stringp (pin-name self)) (string= (pin-name self) (pin-name other))))
       (eq (type-of self) (type-of other))
       (eq (pin-parent self) (pin-parent other)))))

(defmethod deliver-event ((self input-pin) (e e/event:event))
  (let ((part (pin-parent self)))
    (push e (e/part::input-queue part))))

(defmethod deliver-event ((self output-pin) (e e/event:event))
  (let ((part (pin-parent self)))
    (push e (e/part::output-queue part))))

(defmethod ensure-sanity (schem (self pin))
  (e/schematic::ensure-sanity schem (pin-parent self)))

(defmethod get-part ((self pin))
  (pin-parent self))