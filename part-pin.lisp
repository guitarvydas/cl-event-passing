(in-package :e/part-pin)

(defclass pair ()
  ((part :initarg :part :accessor part)
   (pin  :initarg :pin  :accessor pin))) ;; keywords as pin names

(defmethod make-pair ((part e/part:part) (pin e/pin:pin))
  (make-instance 'pair :part part :pin pin))

(defmethod make-pair ((self-part (eql nil)) (pin e/pin:pin))
  (make-instance 'pair :part nil :pin pin))

