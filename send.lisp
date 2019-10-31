(in-package :e/send)

(defgeneric send (e/part:part e/message:message)
  (:documentation "queue the output message on self's output queue - using OUTPUT pin names
   the messages in the queue will be released by the dispatcher"))

(defmethod send ((self e/part:part) (msg e/message:message))
  (e/part:ensure-message-contains-valid-output-pin self msg)
  (e/part:push-input msg))

