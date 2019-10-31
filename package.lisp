(defpackage :cl-event-passing
  (:use :cl)
  (:nicknames :e)
  (:export
   #:hello))

(defpackage :e/pin-bag
  (:use :cl)
  (:export
   #:make-empty-bag
   #:pin-bag
   #:from-list
   #:as-list
   #:ensure-member))

(defpackage :e/part
  (:use :cl)
  (:export
   #:part
   #:push-input
   #:has-input-p
   #:react
   #:has-first-time-p
   #:first-time-function
   #:outqueue-as-list
   #:outputs-as-list
   #:ensure-message-contains-valid-input-pin
   #:ensure-message-contains-valid-output-pin))

(defpackage :e/schematic
  (:use :cl)
  (:export
   #:make-schematic
   #:schematic
   #:wiring
   #:instances
   #:add-instance
   #:add-child-wire
   #:add-input-wire))

(defpackage :e/leaf
  (:use :cl)
  (:export
   #:leaf
   #:make-leaf))

(defpackage :e/message
  (:use :cl)
  (:export
   #:make-message
   #:message
   #:pin
   #:data
   #:clone-with-pin))
   
(defpackage :e/dispatch
  (:use :cl)
  (:export
   #:clear-parts
   #:Start-Dispatcher
   #:remember-part))

(defpackage :e/wire-list
  (:use :cl)
  (:export
   #:make-wire-list
   #:wire-list
   #:add-wire
   #:find-wire))

(defpackage :e/wire
  (:use :cl)
  (:export
   #:make-wire
   #:wire
   #:ins
   #:outs
   #:outs-as-list
   #:deliver-message
   #:member-of-inputs-p))

(defpackage :e/part-pin
  (:use :cl)
  (:export
   #:pair
   #:make-pair))

(defpackage :e/send
  (:use :cl)
  (:export
   #:send))

(defpackage :e/receive
  (:use :cl)
  (:export
   #:respond
   #:inject))

(defpackage :e/queue
  (:use :cl)
  (:export
   #:make-queue
   #:queue
   #:q-push
   #:q-pop
   #:as-list))

(defpackage :e/pin
  (:use :cl)
  (:export
   #:pin
   #:make-pin
   #:pin-equal
   #:as-symbol))

(defpackage :e/pin-wire
  (:use :cl)
  (:export
   #:pin-wire
   #:make-pin-wire))
