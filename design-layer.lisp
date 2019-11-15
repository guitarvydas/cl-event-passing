(in-package :e/part)

(defclass implementable ()
  ((implementation :accessor implementation)
   (debug-name :accessor debug-name :initarg :debug-name)))

(defclass event (implementable)
  ((tag :accessor tag :initarg :tag)
   (data :accessor data :initarg :data)))

(defclass wire (implementable)
  ((receivers :accessor receivers :initform nil)) ;; a list of Receivers (see Implementation-layer.lisp)

(defclass part (implementable)
  ((input-queue :accessor input-queue :initform nil) ;; a list of incoming Events
   (output-queue :accessor output-queue :initform nil) ;; a list of outgoing Events
   (busy-flag :accessor busy-flag :initform nil)
   (namespace-input-pins :accessor namespace-input-pins :initform nil) ;; a list of pin symbols
   (namespace-output-pins :accessor namespace-output-pins :initform nil) ;; a list of pin symbols
   (input-handler :accessor input-handler :initform nil) ;; nil or a function
   (first-time-handler :accessor input-handler :initform nil))) ;; nil or a function

(defclass terminal (part) ())

(defclass schematic (part)
  ((internal-wires :accessor internal-wires :initform nil) ;; a list of Wires
   (internal-parts :accessor internal-parts :initform nil))) ; a list of Parts



(defgeneric @send (self e))

(defgeneric @add-receiver-to-wire (self part pin))

(defgeneric @busy-p (self))

(defgeneric @set-input-handler (self func))

(defgeneric @set-output-handler (self func))

(defgeneric @set-first-time-handler (self))

(defgeneric @start-dispatcher ())
