(in-package :e/wire)

(defmethod make-wire (&key (receivers cl:CONS))
  (make-instance 'wire :receivers receivers))

(defmethod deliver-message ((schem e/schematic:schematic) (wire (eql nil)) (message e/message:message)) )

(defmethod deliver-message ((schem e/schematic:schematic) (wire e/wire:wire) (message e/message:message))
  ;; we don't copy messages/data - receiver must copy the message-data if it intends to mutate it
  (mapc #'(lambda (part-pin)
            (let ((destination-part (e/part-pin:part part-pin))
                  (destination-pin  (e/part-pin:pin  part-pin))
                  (data (e/message:data message)))
              (e/schematic:push-input schem destination-part (e/message:make-message destination-pin data))))
        (receivers wire)))
  
