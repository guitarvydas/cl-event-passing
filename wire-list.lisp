(in-package :e/wire-list)

(defclass wire-list ()
  ((wires :accessor wires :initform nil :initarg :wires)))

(defmethod add-wire ((self wire-list) (wire e/wire:wire))
  (push wire (wires self)))

(defmethod make-wire-list ()
  (make-instance 'wire-list))

(defmethod find-wire ((self wire-list) (pin e/pin:pin))
  (let ((wires (wires self)))
    (@:loop
      (@:exit-when (null wires))
      (let ((wire (pop wires)))
        (when (e/wire:member-of-inputs-p wire pin)
          (return-from find-wire wire)))))
  nil)
