(in-package :cl-event-passing-user)

(defparameter *top-level-part* nil)

;; internal method (not API)
(defmethod is-top-level-p ((part e/part:part))
  (assert *top-level-part*)
  (eq part *top-level-part*))


;; API...

(defun @new-schematic (&key (name "") (input-pins nil) (output-pins nil))
  (let ((schem (e/schematic::new-schematic :name name)))
    (setf (e/part:namespace-input-pins schem) input-pins)
    (setf (e/part:namespace-output-pins schem) output-pins)
    (setf (e/part:input-handler schem) #'e/schematic::schematic-input-handler)
    schem))

(defun @new-code (&key (name "") (input-pins nil) (output-pins nil))
  (let ((part (e/part::new-code :name name)))
    (setf (e/part:namespace-input-pins part) input-pins)
    (setf (e/part:namespace-output-pins part) output-pins)
    part))

(defun @new-wire (&key (name ""))
  (e/wire::new-wire :name name))

(defun @new-event (&key (pin nil) (data nil))
  (e/event::new-event :pin pin :data data))

(defun @initialize ()
  (e/dispatch::reset))

(defmethod @top-level-schematic ((schem e/schematic:schematic))
  (setf *top-level-part* schem)
  (e/dispatch::memo-part schem))

(defmethod @set-first-time-handler ((part e/part:part) fn)
  (setf (e/part::first-time-handler part) fn))

(defmethod @set-input-handler ((part e/part:part) fn)
  (setf (e/part::input-handler part) fn))

(defmethod @add-inbound-receiver-to-wire ((wire e/wire:wire) (part e/part:part) pin-sym)
  (let ((rcv (e/receiver::new-inbound-receiver :part part :pin pin-sym)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod @add-outbound-receiver-to-wire ((wire e/wire:wire) (part e/part:part) pin-sym)
  (let ((rcv (e/receiver::new-outbound-receiver :part part :pin pin-sym)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod @add-source-to-schematic ((schem e/schematic:schematic) (part e/part:part) pin-sym (wire e/wire:wire))
  (let ((s (e/source::new-source :part part :pin pin-sym :wire wire)))
    (e/schematic::ensure-source-not-already-present schem s)
    (e/schematic::add-source schem s)))

(defmethod @add-part-to-schematic ((schem e/schematic:schematic) (part e/part:part))
  (e/schematic::ensure-part-not-already-present schem part)
  (e/schematic::add-part schem part)
  (e/dispatch::memo-part part))

(defmethod @send ((self e/part:part) (e e/event:event))
  (push e (e/part:output-queue self)))

(defmethod @inject ((part e/part:part) pin-sym data)
  (let ((e (e/event::new-event :pin pin-sym :data data)))
    (run-first-times)
    (push e (e/part:input-queue part))
    (e/dispatch::dispatch-single-input)
    (run-dispatcher)))

(defun @start-dispatcher ()
  (run-first-times)
  (run-dispatcher))

(defun run-first-times ()
  (e/dispatch::run-first-times))

(defun run-dispatcher ()
  (@:loop
   (e/dispatch::dispatch-output-queues)
   (@:exit-when (e/dispatch::all-parts-have-empty-input-queues-p))
   (e/dispatch::dispatch-single-input)))

