(in-package :e/receiver)

;; a Receiver is an object stored in a Wire, a Wire can hold multiple Receivers
;; an event is delivered to all Receivers on a Wire "at the same time"

;; a Receiver stores a unique pair - the Part and the Pin (of that Part) which will receive an Event.

;; There are 2 cases for receiving events
;; 1.  A Receiver (child (used to be inbound)) delivers the event to the input queue of a Code Part or a Schematic Part.
;; 2.  A Receiver (self (used to be outbound)) delivers the event to the output queue of its enclosing Schematic Part.

(defclass receiver ()
  ((receiver-pin  :accessor receiver-pin  :initarg :receiver-pin)))

(defclass child-receiver (receiver) ())

(defclass self-receiver (receiver) ())

(defun new-child-receiver (&key (pin nil))
  (make-instance 'child-receiver :receiver-pin pin))

(defun new-self-receiver (&key (pin nil))
  (make-instance 'self-receiver :receiver-pin pin))


;; two receivers are equal if they have the same type, same part and same pin symbol

(defmethod receiver-equal ((r1 child-receiver) (r2 child-receiver))
  (e/pin::pin-equal (receiver-pin r1) (receiver-pin r2)))

(defmethod receiver-equal ((r1 self-receiver) (r2 self-receiver))
  (e/pin::pin-equal (receiver-pin r1) (receiver-pin r2)))

(defmethod receiver-equal ((r1 receiver) (r2 receiver))
  nil)

(defmethod receiver-part ((r receiver))
  (e/pin::get-part (receiver-pin r)))

;; At this point, the Event contains the originating output pin.  The pin must
;; be rewritten to match that of the receiving pin, and the newly-created event is pushed
;; onto the input queue of the receiving part.

(defmethod deliver-event ((r child-receiver) (e e/event:event))
  (let ((new-e (e/event::new-event :event-pin (receiver-pin r) :data (e/event:data e))))
    (push new-e (e/part:input-queue (receiver-part r)))))


;; At this point, the Event contains the originating output pin.  The pin must
;; be rewritten to match that of the receiving pin, and the newly-created event is pushed
;; onto the output queue of the enclosing schematic.

(defmethod deliver-event ((r self-receiver) (e e/event:event))
  (let ((new-e (e/event::new-event :event-pin (receiver-pin r) :data (e/event::data e))))
    (push new-e (e/part:output-queue (receiver-part r)))))

(defmethod ensure-receiver-sanity (schem (self receiver))
  (e/pin::ensure-sanity schem (receiver-pin self)))
