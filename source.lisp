(in-package :e/source)

(defclass source ()
  ((part :accessor part :initarg :part)
   (pin  :accessor pin  :initarg :pin)
   (wire :accessor wire :initarg :wire)))

(defun new-source (&key (part nil) (pin nil) (wire nil))
  (make-instance 'source :part part :pin pin :wire wire))

(defmethod equal-part-pin-p ((self source) (part e/part:part) pin-sym)
  (and (equal (part self) part)
       (equal (pin self)  pin-sym)))

(defmethod deliver-event ((self source) (e e/event:event))
  (e/wire::deliver-event (wire self) e))
