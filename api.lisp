(in-package :cl-event-passing-user)

(defparameter *top-level-part* nil)

;; internal method (not API)
(defmethod is-top-level-p ((part e/part:part))
  (assert *top-level-part*)
  (eq part *top-level-part*))


;; API...

(defun @new-schematic (&key (name "") (input-pins nil) (output-pins nil))
  (let ((schem (e/schematic::new-schematic :name name :input-pins input-pins :output-pins output-pins)))
    schem))

(defun @new-code (&key (name "") (input-pins nil) (output-pins nil) (input-handler nil))
  (let ((part (e/part::new-code :name name)))
    (setf (e/part:namespace-input-pins part) (e/part::make-in-pins part input-pins))
    (setf (e/part:namespace-output-pins part) (e/part::make-out-pins part output-pins))
    (setf (e/part:input-handler part) input-handler)
    part))

(defun @new-wire (&key (name ""))
  (e/wire::new-wire :name name))

(defun @new-event (&key (event-pin nil) (data nil))
  (e/event::new-event :event-pin event-pin :data data))

(defun @initialize ()
  (e/dispatch::reset))

(defmethod @top-level-schematic ((schem e/schematic:schematic))
  (setf *top-level-part* schem)
  (e/dispatch::memo-part schem))

(defmethod @set-first-time-handler ((part e/part:part) fn)
  (setf (e/part::first-time-handler part) fn))

(defmethod @set-input-handler ((part e/part:part) fn)
  (setf (e/part::input-handler part) fn))

(defmethod @add-receiver-to-wire ((wire e/wire:wire) (pin e/pin:pin))
  (if (e/pin::input-p pin)
      (-@add-inbound-receiver-to-wire wire (e/pin::pin-parent pin) pin)
      (-@add-outbound-receiver-to-wire wire (e/pin::pin-parent pin) pin)))

;; -@ means deprecated - we've created a smarter (non-atomic) api call (using more atomic -@ calls)
(defmethod -@add-inbound-receiver-to-wire ((wire e/wire:wire) (part e/part:part) pin)
  (let ((rcv (e/receiver::new-inbound-receiver :part part :pin pin)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod -@add-outbound-receiver-to-wire ((wire e/wire:wire) (part e/part:part) pin)
  (let ((rcv (e/receiver::new-outbound-receiver :part part :pin pin)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod @add-source-to-schematic ((schem e/schematic:schematic) (pin e/pin:pin) (wire e/wire:wire))
  (let ((s (e/source::new-source :part (e/pin:pin-parent pin) :pin pin :wire wire)))
    (e/schematic::ensure-source-not-already-present schem s)
    (e/schematic::add-source schem s)))

(defmethod @add-part-to-schematic ((schem e/schematic:schematic) (part e/part:part))
  (e/schematic::ensure-part-not-already-present schem part)
  (e/schematic::add-part schem part)
  (e/dispatch::memo-part part))

(defmethod @send ((self e/part:part) out-pin out-data)
  (let ((e (@new-event :event-pin out-pin :data out-data)))
    (push e (e/part:output-queue self))))

(defmethod @inject ((part e/part:part) pin-sym data)
  (unless (eq part *top-level-part*)
    (error (format nil "Should not call @inject on anything but the top level part ~S, but @inject(~S) is being called."
                   *top-level-part* part)))
  (let ((e (e/event::new-event :event-pin pin-sym :data data)))
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

