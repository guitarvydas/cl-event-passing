(in-package :e/source)

(defclass source ()
  ((source-pin :accessor source-pin  :initarg :source-pin)
   (wire :accessor wire :initarg :wire)
   (debug-name :accessor debug-name :initarg :debug-name :initform "")))

(defmethod clone-with-parent ((cloned-parent e/part:part) (proto source))
  (make-instance 'source
                :source-pin (e/pin::clone-with-parent cloned-parent (source-pin proto))
                :wire (e/wire::clone-with-parent cloned-parent (wire proto))
                :debug-name (format nil "cloned source ~S" (debug-name proto))))

(defun new-source (&key (pin nil) (wire nil))
  (make-instance 'source :source-pin pin :wire wire))

(defmethod source-pin-equal ((self source) (pin e/pin:pin))
  (e/pin::pin-equal (source-pin self)  pin))

(defmethod deliver-event ((self source) (e e/event:event))
  (e/wire::deliver-event (wire self) e))

(defmethod set-wire-of-source ((self source) (wire e/wire:wire))
  (setf (wire self) wire))

(defmethod ensure-source-part-exists-in-schematic (schem (self source))
  (unless (eq schem (source-pin self)) ;; could be a pin of self
    (e/pin::ensure-sanity schem (source-pin self))))

(defmethod ensure-source-sanity (schem (self source))
  (unless (eq schem (source-pin self)) ;; could be a pin of self
    (ensure-source-part-exists-in-schematic schem self)
    (e/wire::ensure-wire-sanity schem (wire self))))
  