(in-package :e/message)

(defclass message ()
  ((pin :accessor pin :initarg :pin) ;; symbols (e.g. keywords) as pin names
   (data :accessor data :initarg :data)))

(defmethod clone-with-pin ((self message) (pin e/pin:pin))
  (make-instance 'message :pin pin :data (data self)))

(defmethod make-message ((pin e/pin:pin) (data T))
  (let ((pin (e/pin:make-pin sym)))
    (make-instance 'message :pin pin :data data)))
  
