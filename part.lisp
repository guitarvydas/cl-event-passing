(in-package :e/part)

(defclass part ()
  ((input-queue :accessor input-queue :initform nil) ;; a list of incoming Events
   (output-queue :accessor output-queue :initform nil) ;; a list of outgoing Events
   (busy-flag :accessor busy-flag :initform nil)
   (namespace-input-pins :accessor namespace-input-pins :initform nil) ;; a list of pin symbols
   (namespace-output-pins :accessor namespace-output-pins :initform nil) ;; a list of pin symbols
   (input-handler :accessor input-handler :initform nil) ;; nil or a function
   (first-time-handler :accessor first-time-handler :initform nil) ;; nil or a function
   (parent-schem :accessor parent-schem :initform nil :initarg :parent-schem)
   (debug-name :accessor debug-name :initarg :name :initform ""))) ;; for debug

(defclass code (part) ())

(defun new-code (&key (name "") (input-pins nil) (output-pins nil))
  (let ((self (make-instance 'code :name name)))
    (setf (e/part:namespace-input-pins self) input-pins)
    (setf (e/part:namespace-output-pins self) output-pins)
    self))

(defmethod name ((p part))
  (if (string= "" (debug-name p))
      p
    (debug-name p)))

(defmethod busy-p (self code)
  (busy-flag self))

(defmethod has-input-queue-p (self part)
  (not (null (input-queue self))))

(defmethod has-output-queue-p (self part)
  (not (null (output-queue self))))
