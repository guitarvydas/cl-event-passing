(in-package :e/schematic)

(defclass schematic (e/part:part)
  ((wires :accessor wires :initarg :wires)
   (child-wire-map :accessor child-wire-map :initarg :child-wire-map :initform (make-hash-table)) ;; pointers to wires between children parts (and self outputs)
   (input-wire-map :accessor input-wire-map :initarg :input-wire-map :initform (make-hash-table)) ;; pointers to wires from each of this schematic's inputs
   (instances :accessor instances :initform nil :initarg :instances)) ;; list of children parts
  (:default-initargs
   :reactor #'schematic-reactor))

(defun make-schematic ()
  (make-instance 'schematic))

(defmethod make-slot-for-each-output ((child e/part:part))
  (let ((hmap (make-hash-table)))
    (mapc #'(lambda (output-sym)
              (setf (gethash output-sym hmap) nil))
          (e/part:output-pins-as-list child))
    hmap))

(defmethod add-instance ((self schematic) (instance e/part:part))
  (push instance (instances self))
  (setf (gethash instance (child-wire-map self))
        (make-slot-for-each-output instance)))

(defun ensure-no-wire (hmap pin)
  (multiple-value-bind (val success)
      (gethash pin hmap)
    (cl:assert (and success (null val)))))

(defmethod add-child-wire ((self schematic) (child-instance e/part:part) (child-output-pin e/pin:pin) (wire e/wire:wire))
  (multiple-value-bind (child-map success)
      (gethash child-instance (child-wire-map self))
    (cl:assert success)
    (ensure-no-wire (child-wire-map child-output-pin))
    (setf (gethash child-output-pin child-wire-map)
          wire)))

(defmethod add-input-wire ((self schematic) (input-pin e/pin:pin) (wire e/wire:wire))
  (ensure-no-wire (input-wire-map self) pin)
  (setf (gethash pin (input-wire-map self))
        wire))

(defmethod find-wire-for-input-pin (in-map (pin e/pin:pin))
  (multiple-value-bind (wire success)
      (gethash pin in-map)
    (assert success)
    wire))

(defmethod schematic-reactor ((self schematic) (msg e/message:message))
  "react to a single input message to a schematic - push the message inside the schematic
   to all parts attached to given input pin"
  (e/part:ensure-message-contains-valid-input-pin self msg)
  (let ((in-map (input-wire-map self))
        (schematic-input-pin (e/message:pin message)))
    (let ((wire (find-wire-for-input-pin in-map pin)))
      (e/wire:deliver-message wire message))))

(defmethod find-wire-for-pin-inside-schematic ((schem e/schematic:schematic) (child e/part:part) (child-out-pin e/pin:pin))
  (multiple-value-bind (child-part-output-map success)
      (gethash child (child-wire-map schem))
    (assert success)
    (multiple-value-bind (wire success)
        (gethash child-out-pin child-part-output-map)
      (assert succes)
      wire)))


