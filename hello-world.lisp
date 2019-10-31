(in-package :cl-event-passing)

(defun hello ()
  ;; lots of mundane, very explicit, bookkeeping here
  ;; - this is where a diagram compiler might help cut down on syntactic noise
  ;; - (in fact, if I hadn't started out with diagrams, I might never have arrived at this combination of features)
  (let ((schem (e/schematic:make-schematic))
        (sender (e/leaf:make-leaf
                 :first-time #'start-sender
                 :out-pins (e/pin-bag:from-list '(:out))))
        (receiver (e/leaf:make-leaf
                   :reactor #'receiver-display
                   :in-pins (e/pin-bag:from-list '("in")))))
    (e/schematic:add-instance sender)
    (e/schematic:add-instance receiver)
    (let ((receiver-pair (e/part-pin:make-pair receiver (cl-message/pin:make-pin :in))))
      (let ((wire (e/wire:make-wire :receivers (list receiver-pair))))
        (e/schematic:add-child-wire wire))))
  (e/dispatch:Start-Dispatcher))


;; code / callbacks

(defun receiver-display ((msg e/message))
  (case (e/pin:as-symbol (e/message:pin message))

    (:print
     (cl:print (e/message:data msg)))

    (otherwise
     (error (format nil "unsupported message sent to e:display /~S/" msg)))))

(defun start-sender ((self e:part))
  (e/send:send (e/message:make-message :outpin "Hello")))
     
