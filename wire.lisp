(in-package :e/wire)

;; a wire is a collection of destination part-pin pairs (:receivers), a wire is pointed-to by any number of outputs

(defclass wire ()
  ((receivers :accessor receivers :initarg :receivers))) ;; list of part-pin pairs

(defmethod make-wire (&key (receivers cl:CONS))
  (make-instance 'wire :receivers receivers))

(defmethod deliver-message ((wire (eql nil)) (message e/message:message))
  )

(defmethod deliver-message ((wire e/wire:wire) (message e/message:message))
  ;; we don't copy messages/data - receiver must copy the message-data if it intends to mutate it
  (mapc #'(lambda (part-pin)
            (let ((destination-part (e/part-pin:part part-pin))
                  (destination-pin  (e/part-pin:pin  part-pin))
                  (data (e/message:data message)))
              (cl-messsaging/part:push-input destination-part (make-message destination-pin data))))
        wire))
  
