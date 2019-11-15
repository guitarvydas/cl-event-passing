(defpackage cl-event-passing
  (:nicknames :a)
  (:use :cl))

(defpackage cl-event-passing-user
  ;; top level API
  (:use :cl)
  (:nicknames :aa)
  (:export
   #:@new-schematic
   #:@new-code
   #:@new-wire
   #:@new-event
   #:@initialize
   #:@top-level-schematic
   #:@set-first-time-handler
   #:@set-input-handler
   #:@add-inbound-receiver-to-wire
   #:@add-outbound-receiver-to-wire
   #:@add-source-to-schematic
   #:@add-part-to-schematic
   #:@send
   #:@inject
   #:@start-dispatcher))

(defpackage cl-event-passing-part
  (:nicknames :e/part)
  (:use :cl)
  (:export
   #:part
   #:code
   #:name
   #:input-queue
   #:has-input-queue-p
   #:output-queue
   #:has-output-queue-p
   #:busy-flag
   #:namespace-input-pins
   #:namespace-output-pins
   #:input-handler
   #:first-time-handler
   #:parent-schem))

(defpackage cl-event-passing-event
  (:nicknames :e/event)
  (:use :cl)
  (:export
   #:event
   #:new-event
   #:pin
   #:data))

(defpackage cl-event-passing-source
  (:nicknames :e/source)
  (:use :cl)
  (:export
   #:source
   #:part
   #:pin
   #:wire))

(defpackage cl-event-passing-receiver
  (:nicknames :e/receiver)
  (:use :cl)
  (:export
   #:part
   #:pin
   #:receiver
   #:inbound-receiver
   #:outbound-receiver))

(defpackage cl-event-passing-schematic
  (:nicknames :e/schematic)
  (:use :cl)
  (:export
   #:name
   #:schematic
   #:internal-parts
   #:internal-wires
   #:input-queue
   #:output-queue
   #:busy-flag
   #:namespace-input-pins
   #:namespace-output-pins
   #:input-handler
   #:first-time-handler
   #:parent-schem))

(defpackage cl-event-passing-dispatch
  (:nicknames :e/dispatch)
  (:use :cl))

(defpackage cl-event-passing-user-util
  (:nicknames :e/util)
  (:use :cl)
  (:export
   #:ensure-not-in-list))

(defpackage cl-event-passing-user-wire
  (:nicknames :e/wire)
  (:use :cl)
  (:export
   #:wire
   #:receivers))
