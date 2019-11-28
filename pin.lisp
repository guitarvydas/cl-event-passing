(in-package :e/pin)

(defclass pin ()
  ((pin-name :accessor pin-name :initarg :pin-name)
   (pin-parent :accessor pin-parent :initarg :pin-parent)
   (debug-name :accessor debug-name :initarg :debug-name :initform "")))
  
(defclass input-pin (pin) ())

(defclass output-pin (pin) ())

(defmethod print-object ((obj input-pin) out)
  (format out "<input-pin[~a/~a/~a]>" (e/part::name (pin-parent obj)) (pin-name obj) (debug-name obj)))

(defmethod print-object ((obj output-pin) out)
  (format out "<output-pin[~a/~a/~a]>" (e/part::name (pin-parent obj)) (pin-name obj) (debug-name obj)))

(defmethod clone-with-part (cloned-part (proto input-pin))
  (make-instance 'input-pin
                 :pin-name (pin-name proto)
                 :pin-parent cloned-part
                 :debug-name (format nil "cloned input pin ~S" (debug-name proto))))

(defmethod clone-with-part (cloned-part (proto output-pin))
  (make-instance 'output-pin
                 :pin-name (pin-name proto)
                 :pin-parent cloned-part
                 :debug-name (format nil "cloned output pin ~S" (debug-name proto))))

(defmethod dup-pin (proto-map cloned-map (proto input-pin))
  (let ((proto-part (pin-parent proto)))
    (let ((cloned-part (e/part::map-part proto-part proto-map cloned-map)))
      (e/part::get-input-pin cloned-part (pin-name proto)))))

(defmethod dup-pin (proto-map cloned-map (proto output-pin))
  (let ((proto-part (pin-parent proto)))
    (let ((cloned-part (e/part::map-part proto-part proto-map cloned-map)))
      (e/part::get-output-pin cloned-part (pin-name proto)))))

(defmethod input-p ((self pin))
  (eq 'input-pin (type-of self)))

(defmethod output-p ((self pin))
  (eq 'output-pin (type-of self)))

(defun new-pin (&key (pin-name "") (direction :in) pin-parent) ;; parent is unbound by default - always an error if parent is not bound (later)
  (make-instance
   (if (eq :input direction)
       'input-pin
     (if (eq :output direction)
         'output-pin
       (error (format nil "pin direction must be specified as :input or :output, but ~S was given" direction))))
   :pin-name pin-name
   :pin-parent pin-parent))

(defmethod pin-equal ((self pin) (other pin))
  (or (eq self other)
      (and
       (or (and (symbolp (pin-name self)) (equal (pin-name self) (pin-name other)))
           (and (stringp (pin-name self)) (string= (pin-name self) (pin-name other))))
       (eq (type-of self) (type-of other))
       (eq (pin-parent self) (pin-parent other)))))

(defmethod deliver-event ((self input-pin) (e e/event:event))
  (let ((part (pin-parent self)))
    (push e (e/part::input-queue part))))

(defmethod deliver-event ((self output-pin) (e e/event:event))
  (let ((part (pin-parent self)))
    (push e (e/part::output-queue part))))

(defmethod ensure-sanity (schem (self pin))
  (e/schematic::ensure-sanity schem (pin-parent self)))

(defmethod get-part ((self pin))
  (pin-parent self))