(in-package :e/part)

(defclass part ()
  ((inqueue :accessor inqueue :initform (e/queue:make-queue))
   (outqueue :accessor outqueue :initform (e/queue:make-queue))
   (in-pins :accessor in-pins :initarg :in-pins)  ;; list of input pins for this part (for checking legal receives)
   (out-pins :accessor out-pins :initarg :out-pins) ;; list of output pins for this part (for checking legal sends)
   (parent :accessor parent :initform nil :initarg :parent)
   (reactor :accessor reactor :initform nil :initarg :reactor) ;; reactor function / callback (self msg)
   (first-time-function :accessor first-time-function :initarg :first-time :initform nil))) ;; function((self part))
  
   ;; Essay: Outqueue might seem superfluous, but don't optimize it away - it is
   ;; one of the essential elements of the act of: snipping dependencies and making parts
   ;; be truly *concurrent* (asynchronous).  Parts MUST NOT refer to their
   ;; peers (this removes possible dependencies).  Parts are WIRED to peers
   ;; via the wiring list contained in their parent (always a schematic or NIL).  Parts
   ;; can be wired only to peer parts contained in the same schematic
   ;; (hierarchy ==> simplicity ==> scalability) or to output pins
   ;; of the parent schematic

(defmethod initialize-instance :after ((self part) &key)
  (e/dispatch:remember-part self))

(defmethod set-parent ((self part) (parent part))
  (setf (parent self) parent))

(defmethod has-first-time-p ((self part))
  (not (null (first-time-function self))))

(defmethod ensure-is-input-pin ((self part) (pin-sym symbol))
  (e/pin-collection:ensure-member (in-pins self) pin-sym))

(defmethod ensure-is-output-pin ((self part) (pin-sym symbol))
  (e/pin-collection:ensure-member (out-pins self) pin-sym))

(defmethod ensure-message-contains-valid-input-pin ((self part) (msg e/message:message))
  (let ((pin (e/message:pin msg)))
    (ensure-is-input-pin self pin)))

(defmethod ensure-message-contains-valid-output-pin ((self part) (msg e/message:message))
  (let ((pin (e/message:pin msg)))
    (ensure-is-output-pin self pin)))

(defmethod react ((self part) (msg e/message:message))
  (funcall (reactor part) part msg))

(defmethod push-input ((self part) (msg e/message:message))
  (ensure-message-contains-valid-input-pin self msg)
  (e/queue:q-push (inqueue self) msg))

(defmethod has-input-p ((self part))
  (not (null (inqueue self))))

(defmethod pop-input ((self part))
  (e/queue:q-pop (inqueue self)))

(defmethod outqueue-as-list ((self part))
  (e/queue:as-list))

(defmethod output-pins-as-list ((self part))
  (e/pin-collection:as-list (out-pins self)))

(defmethod lookup-output-pin ((self part) (pin-sym symbol))
  (ensure-is-output-pin self pin-sym)
  (e/pin-collection:lookup-pin (out-pins self) pin-sym))
