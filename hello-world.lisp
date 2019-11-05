;; test todo: test that wire from schem input pin to schem output pin works
;; test todo: test that wire from schem input pin can split

(in-package :cl-event-passing)

(defun hello ()
  ;; lots of mundane, very explicit, bookkeeping here
  ;; - this is where a diagram compiler might help cut down on syntactic noise
  ;; - (in fact, if I hadn't started out with diagrams, I might never have arrived at this combination of features)
  (e/dispatch:reset-dispatcher)
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

(defun hello2 ()
  (e/dispatch:reset-dispatcher)
  ;; wire straight from sender to output of schematic
  (let ((schem (e/schematic:make-schematic :out-pins (e/pin-collection:from-list '(:schem-out))))
        (sender (e/leaf:make-leaf
                 :first-time #'start-sender
                 :out-pins (e/pin-collection:from-list '(:out)))))
    (e/schematic:add-instance schem sender)
    (let ((receiver-pair (e/part-pin:make-pair nil (e/part:lookup-output-pin schem :schem-out))))
      (let ((wire (e/wire:make-wire :receivers (list receiver-pair))))
	;; wire is the wire between the sender's output (:out) and the schematic's output (:schem-out)
	;; wire is added to the schematic's map, as output from sender's :out pin
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire))))
  (e/dispatch:Start-Dispatcher))

(defun hello3 ()
  (e/dispatch:reset-dispatcher)
  ;; wire straight from first-time of schematic to output of schematic
  (let ((schem (e/schematic:make-schematic
                :first-time #'start-schematic-sender
                :out-pins (e/pin-collection:from-list '(:schem-out)))))
    (e/dispatch:Start-Dispatcher)))

(defun hello4 ()
  ;; schematic has an input :schem-in, injecting into that pin goes to sender, which then goes to receiver
  (e/dispatch:reset-dispatcher)
  (let ((schem (e/schematic:make-schematic
                :in-pins (e/pin-collection:from-list '(:schem-in))))
        (sender (e/leaf:make-leaf
                 :reactor #'sender-react
                 :in-pins (e/pin-collection:from-list '(:in))
                 :out-pins (e/pin-collection:from-list '(:out))))
        (receiver (e/leaf:make-leaf
                   :reactor #'receiver-display
                   :in-pins (e/pin-collection:from-list '(:in)))))
    (e/schematic:add-instance schem sender)
    (e/schematic:add-instance schem receiver)
    (let ((receiver-pair (e/part-pin:make-pair receiver (e/part:lookup-input-pin receiver :in)))
          (sender-input-pair (e/part-pin:make-pair sender (e/part:lookup-input-pin sender :in))))
      (let (;; wire is the wire between the sender's output (:out) and the receiver's input (:in)
            ;; wire is added to the schematic's map, as output from sender's :out pin
            (wire (e/wire:make-wire :receivers (list receiver-pair)))
            
            ;; input-wire comes from the input of the shematic (:schem-in) to the input of the
            ;; sender part (:in (NB :in is also the name of the input pin of the receiver part))
            (input-wire (e/wire:make-wire :receivers (list sender-input-pair))))
        
        (e/schematic:add-child-wire schem sender (e/part:lookup-output-pin sender :out) wire)
        (e/schematic:add-self-input-wire schem (e/part:lookup-input-pin schem :schem-in) input-wire)))
    (e/send:inject schem (e/message:make-message (e/part:lookup-input-pin schem :schem-in) "hello4"))))

(defun hello5 ()
  ;; schematic has an input :schem-in, injecting into that pin goes to its own output :schem-out
  (e/dispatch:reset-dispatcher)
  (let ((schem (e/schematic:make-schematic
                :in-pins (e/pin-collection:from-list '(:schem-in))
                :out-pins (e/pin-collection:from-list '(:schem-out)))))
    (let ((self-receiver-pair (e/part-pin:make-pair nil (e/part:lookup-output-pin schem :schem-out))))
      (let (;; wire is the wire between self's input (:schem-in) and self's output (:schem-out)
            (wire (e/wire:make-wire :receivers (list self-receiver-pair))))
        (e/schematic:add-self-input-wire schem (e/part:lookup-input-pin schem :schem-in) wire)))
    (e/send:inject schem (e/message:make-message (e/part:lookup-input-pin schem :schem-in) "hello 5"))))

(defun test ()
  (format *standard-output* "~&running hello~%")
  (hello)
  (format *standard-output* "~&~%running hello2~%")
  (hello2)
  (format *standard-output* "~&~%running hello3~%")
  (hello3)
  (format *standard-output* "~&~%running hello4~%")
  (hello4)
  (format *standard-output* "~&~%running hello5~%")
  (hello5))

;; code / callbacks

(defmethod receiver-display ((self e/part:part) (msg e/message:message))
  (declare (ignore self))

  (case (e/pin:as-symbol (e/message:pin msg))

    (:in
     (cl:format cl:*standard-output* "~&receiver-display output: ~S~%"
                (e/message:data msg)))

    (otherwise
     (error (format nil "unsupported message sent to e:display /~S/" msg)))))

(defmethod rewrite-message-with-my-output ((self e/part:part) (out-sym cl:symbol) (msg e/message:message))
  "change input pin to output pin in copy of message"
  (e/part:ensure-message-contains-valid-input-pin self msg)
  (let ((outmsg (e/message:clone-with-pin msg (e/part:lookup-output-pin self out-sym))))
    (e/part:ensure-message-contains-valid-output-pin self outmsg)
    outmsg))

(defmethod sender-react ((self e/part:part) (msg e/message:message))
  (e/send:send self (rewrite-message-with-my-output self :out msg)))

(defmethod start-sender ((self e/part:part))
  (e/send:send self (e/message:make-message (e/part:lookup-output-pin self :out) "Hello")))
     
(defmethod start-schematic-sender ((self e/schematic:schematic))
  (e/send:send self (e/message:make-message (e/part:lookup-output-pin self :schem-out) "Hello")))
     
