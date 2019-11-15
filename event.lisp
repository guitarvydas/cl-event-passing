(in-package :e/event)

(defclass event ()
  ((pin :accessor pin :initarg :pin)
   (data :accessor data :initarg :data)))

(defun new-event (&key (pin nil) (data nil))
  (make-instance 'event :pin pin :data data))

          