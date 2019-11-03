(in-package :e/wire)

(defmethod make-wire (&key (receivers cl:CONS))
  (make-instance 'wire :receivers receivers))

(defmethod deliver-message ((schem e/schematic:schematic) (wire (eql nil)) (message e/message:message))
  ;; return list of parts that have new outputs
  nil)

(defmethod deliver-message ((schem e/schematic:schematic) (wire e/wire:wire) (message e/message:message))
  ;; we don't copy messages/data - receiver must copy the message-data if it intends to mutate it
  ;; return list of parts that have newly created outputs
  (let ((newly-created-outputs-list nil))
    (mapc #'(lambda (part-pin)
              (let ((destination-part (e/part-pin:part part-pin))
                    (destination-pin  (e/part-pin:pin  part-pin))
                    (data (e/message:data message)))
                (append
                 (e/schematic:push-input schem destination-part (e/message:make-message destination-pin data))
                 newly-created-outputs-list)))
          (receivers wire))
    ;; In most cases, there are no newly created outputs.
    ;; In the other cases where we have delivered messages to the outputs of a schematic, we are creating new
    ;; outputs
     newly-created-outputs-list))
