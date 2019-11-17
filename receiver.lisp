(in-package :e/receiver)

;; a Receiver is an object stored in a Wire, a Wire can hold multiple Receivers
;; an event is delivered to all Receivers on a Wire "at the same time"

;; a Receiver stores a unique pair - the Part and the Pin (of that Part) which will receive an Event.

;; There are 2 cases for receiving events
;; 1.  A Receiver (inbound) delivers the event to the input queue of a Code Part or a Schematic Part.
;; 2.  A Receiver (outbound) delivers the event to the output queue of its enclosing Schematic Part.

(defclass receiver ()
  ((receiver-part :accessor receiver-part :initarg :receiver-part)
   (receiver-pin  :accessor receiver-pin  :initarg :receiver-pin)))

(defclass inbound-receiver (receiver) ())

(defclass outbound-receiver (receiver) ())

(defun new-inbound-receiver (&key (part nil) (pin nil))
  (make-instance 'inbound-receiver :receiver-part part :receiver-pin pin))

(defun new-outbound-receiver (&key (part nil) (pin nil))
  (make-instance 'outbound-receiver :receiver-part part :receiver-pin pin))


;; two receivers are equal if they have the same type, same part and same pin symbol

(defmethod receiver-equal ((r1 inbound-receiver) (r2 inbound-receiver))
  (and (equal (receiver-part r1) (receiver-part r2))
       (equal (receiver-pin r1) (receiver-pin r2))))

(defmethod receiver-equal ((r1 outbound-receiver) (r2 outbound-receiver))
  (and (equal (receiver-part r1) (receiver-part r2))
       (equal (receiver-pin r1) (receiver-pin r2))))

(defmethod receiver-equal ((r1 receiver) (r2 receiver))
  nil)



;; At this point, the Event contains the originating output pin.  The pin must
;; be rewritten to match that of the receiving pin, and the newly-created event is pushed
;; onto the input queue of the receiving part.

(defmethod deliver-event ((r inbound-receiver) (e e/event:event))
  (let ((new-e (e/event::new-event :event-pin (receiver-pin r) :data (e/event:data e))))
    (push new-e (e/part:input-queue (receiver-part r)))))


;; At this point, the Event contains the originating output pin.  The pin must
;; be rewritten to match that of the receiving pin, and the newly-created event is pushed
;; onto the output queue of the enclosing schematic.

(defmethod deliver-event ((r outbound-receiver) (e e/event:event))
  (let ((new-e (e/event::new-event :event-pin (receiver-pin r) :data (e/event::data e))))
    (push new-e (e/part:output-queue (receiver-part r)))))
