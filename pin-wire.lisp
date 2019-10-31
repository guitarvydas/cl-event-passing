(in-package :e/pin-wire)

(defclass pin-wire ()
  ((pin :accessor pin :initarg :pin)
   (wire :accessor wire :initarg :wire)))

(defmethod make-pin-wire ((pin e/pin:pin) (wire e/wire:wire))
  (make-instance 'pin-wire :pin pin :wire wire))
   
          
