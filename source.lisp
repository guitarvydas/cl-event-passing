(in-package :e/source)

(defclass source ()
  ((source-pin :accessor source-pin  :initarg :source-pin)
   (wire :accessor wire :initarg :wire)))

(defun new-source (&key (pin nil) (wire nil))
  (make-instance 'source :source-pin pin :wire wire))

(defmethod source-pin-equal ((self source) (pin e/pin:pin))
  (e/pin::pin-equal (source-pin self)  pin))

(defmethod deliver-event ((self source) (e e/event:event))
  (e/wire::deliver-event (wire self) e))
