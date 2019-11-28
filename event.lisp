(in-package :e/event)

(defclass event ()
  ((event-pin :accessor event-pin :initarg :event-pin)
   (data :accessor data :initarg :data)))

(defun new-event (&key (event-pin nil) (data nil))
  (make-instance 'event :event-pin event-pin :data data))

(defmethod sym ((e e/event:event))
  (e/pin::pin-name (event-pin e)))

