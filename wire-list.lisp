(in-package :e/wire-list)

(defclass wire-list ()
  ((wires :accessor wires :initform nil :initarg :wires)))

(defmethod add-wire ((self wire-list) (wire e/wire:wire))
  (push wire (wires self)))

(defmethod make-wire-list ()
  (make-instance 'wire-list))

