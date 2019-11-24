(in-package :cl-event-passing-user)

(defparameter *top-level-part* nil)

;; internal method (not API)
(defmethod is-top-level-p ((part e/part:part))
  (assert *top-level-part*)
  (eq part *top-level-part*))


;; API...

(defun @new-schematic (&key (name "") (input-pins nil) (output-pins nil) (first-time-handler nil))
  (let ((schem (e/schematic::new-schematic :name name :input-pins input-pins :output-pins output-pins
                                           :first-time-handler first-time-handler)))
    schem))

(defun @new-code (&key (name "") (input-pins nil) (output-pins nil) (input-handler nil) (first-time-handler nil))
  (let ((part (e/part::new-code :name name)))
    (setf (e/part:namespace-input-pins part) (e/part::make-in-pins part input-pins))
    (setf (e/part:namespace-output-pins part) (e/part::make-out-pins part output-pins))
    (setf (e/part:input-handler part) input-handler)
    (setf (e/part:first-time-handler part) first-time-handler)
    part))

(defun @new-wire (&key (name ""))
  (e/wire::new-wire :name name))

(defun @new-event (&key (event-pin nil) (data nil))
  (e/event::new-event :event-pin event-pin :data data))

(defun @initialize ()
  (e/util::reset)
  (e/dispatch::reset))

(defmethod @top-level-schematic ((schem e/part:schematic))
  (setf *top-level-part* schem)  ;; for debugging only
  (e/dispatch::memo-part schem)) ;; always needed

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
  (let ((rcv (e/receiver::new-inbound-receiver :pin pin)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod -@add-outbound-receiver-to-wire ((wire e/wire:wire) (part e/part:part) pin)
  (let ((rcv (e/receiver::new-outbound-receiver :pin pin)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod @add-source-to-schematic ((schem e/part:schematic) (pin e/pin:pin) (wire e/wire:wire))
  (let ((s (e/source::new-source :pin pin :wire wire)))
    (e/schematic::ensure-source-not-already-present schem s)
    (e/schematic::add-source schem s)))

(defmethod @add-part-to-schematic ((schem e/part:schematic) (part e/part:part))
  (e/schematic::ensure-part-not-already-present schem part)
  (e/schematic::add-part schem part)
  (e/dispatch::memo-part part))

(defmethod @send ((self e/part:part) (out SYMBOL) out-data)
  (let ((out-pin (e/part::get-output-pin self out)))
    (@send self out-pin out-data)))

(defmethod @send ((self e/part:part) (pin e/pin:pin) data)
  (if (e/dispatch::dispatcher-active-p);(eq self *top-level-part*)
      (send-output self pin data)
    (progn
      (send-input self pin data)
      (e/dispatch::start-dispatcher))))
  
(defmethod send-output ((self e/part:part) (out-pin e/pin:pin) out-data)
  (let ((e (@new-event :event-pin out-pin :data out-data)))
    (e/util:logging e)
    (push e (e/part:output-queue self))))

(defmethod send-input ((part e/part:part) pin data)
  #+nil(unless (eq part *top-level-part*)
    (error (format nil "Should not call @inject on anything but the top level part ~S, but @inject(~S) is being called."
                   *top-level-part* part)))
  (let ((e (e/event::new-event :event-pin pin :data data)))
    (e/dispatch::run-first-times)
    (e/util:logging e)
    (push e (e/part:input-queue part))
    (e/dispatch::start-dispatcher)))

(defun @start-dispatcher ()
  (e/dispatch::start-dispatcher))

(defun @history ()
  (e/util::get-logging))

(defun @enable-logging ()
  (e/util::enable-logging))