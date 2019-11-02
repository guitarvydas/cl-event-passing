;; test todo: test that wire from schem input pin to schem output pin works
;; test todo: test that wire from schem input pin can split

(in-package :cl-event-passing)

(defun hello ()
  ;; lots of mundane, very explicit, bookkeeping here
  ;; - this is where a diagram compiler might help cut down on syntactic noise
  ;; - (in fact, if I hadn't started out with diagrams, I might never have arrived at this combination of features)
  (let ((schem (e/schematic:make-schematic))
        (sender (e/leaf:make-leaf
                 :first-time #'start-sender
                 :out-pins (e/pin-collection:from-list '(:out))))
        (receiver (e/leaf:make-leaf
                   :reactor #'receiver-display
                   :in-pins (e/pin-collection:from-list '(:in)))))
    (e/schematic:add-instance schem sender)
    (e/schematic:add-instance schem receiver)
    (let ((receiver-pair (e/part-pin:make-pair receiver (e/part:lookup-input-pin receiver :in))))
      (let ((wire (e/wire:make-wire :receivers (list receiver-pair))))
	;; wire is the wire between the sender's output (:out) and the receiver's input (:in)
	;; wire is added to the schematic's map, as output from sender's :out pin
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire))))
  (e/dispatch:Start-Dispatcher))


;; code / callbacks

(defmethod receiver-display ((self e/part:part) (msg e/message:message))
  (declare (ignore self))

  (case (e/pin:as-symbol (e/message:pin msg))

    (:in
     (cl:format cl:*standard-output* "~&receiver-display output: ~S~%"
                (e/message:data msg)))

    (otherwise
     (error (format nil "unsupported message sent to e:display /~S/" msg)))))

(defmethod start-sender ((self e/part:part))
  (e/send:send self (e/message:make-message (e/part:lookup-output-pin self :out) "Hello")))
     
