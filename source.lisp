(in-package :e/source)

(defclass source ()
  ((source-part :accessor source-part :initarg :source-part)
   (source-pin  :accessor source-pin  :initarg :source-pin)
   (wire :accessor wire :initarg :wire)))

(defun new-source (&key (part nil) (pin nil) (wire nil))
  (make-instance 'source :source-part part :source-pin pin :wire wire))

(defmethod equal-part-pin-p ((self source) (part e/part:part) pin)
  (and (eq (source-part self) part)
       (e/pin::pin-equal (source-pin self)  pin)))

(defmethod deliver-event ((self source) (e e/event:event))
  (e/wire::deliver-event (wire self) e))
