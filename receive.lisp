(in-package :e/receive)

(defgeneric respond (e/part:part e/message:message)
  (:documentation "execute a response (a closure/function/callback) when a message arrives in the input queue
   the response may cause 0 or more calls to SEND"))

(defmethod inject ((self e/part:part) (message e/message:message))
  "send a message in from the outside - message already contains correct input :pin
   starts Dispatcher, if not already started"
  (e/part:ensure-message-contains-valid-input-pin self message)
  (e/part:push-input self message)
  (e/dispatch:Start-Dispatcher))

(defmethod respond (e/part:part e/message:message)
  (e/part:ensure-message-contains-valid-input-pin self message)
  (e/part:react part message))
