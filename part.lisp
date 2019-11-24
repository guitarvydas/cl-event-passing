(in-package :e/part)

(defclass part ()
  ((input-queue :accessor input-queue :initform nil) ;; a list of incoming Events
   (output-queue :accessor output-queue :initform nil) ;; a list of outgoing Events
   (busy-flag :accessor busy-flag :initform nil)
   (namespace-input-pins :accessor namespace-input-pins :initform nil) ;; a list of pin objects
   (namespace-output-pins :accessor namespace-output-pins :initform nil) ;; a list of pin objects
   (input-handler :accessor input-handler :initform nil :initarg :input-handler) ;; nil or a function
   (first-time-handler :accessor first-time-handler :initform nil) ;; nil or a function
   (parent-schem :accessor parent-schem :initform nil :initarg :parent-schem)
   (debug-name :accessor debug-name :initarg :name :initform ""))) ;; for debug

(defclass code (part) ())

(defclass schematic (part)
  ((sources :accessor sources :initform nil) ;; a list of Sources (which contain a list of Wires which contain a list of Receivers)
   (internal-parts :accessor internal-parts :initform nil))) ; a list of Parts

(defmethod make-in-pins ((pin-parent part) lis)
  (mapcar #'(lambda (sym-or-pin)
	      (if (or (symbolp sym-or-pin)
                      (stringp sym-or-pin))
		  (e/pin::new-pin :pin-name sym-or-pin :direction :input :pin-parent pin-parent)
		  (if (and (eq 'e/pin:pin (type-of sym-or-pin))
			   (e/pin::input-p sym-or-pin))
		      sym-or-pin
		      (error (format nil "input pin given as ~S but must be a symbol, string or an input pin" sym-or-pin)))))
	  lis))

(defmethod make-out-pins ((pin-parent part) lis)
  (mapcar #'(lambda (sym-or-pin)
	      (if (or (symbolp sym-or-pin)
                      (stringp sym-or-pin))
		  (e/pin::new-pin :pin-name sym-or-pin :direction :output :pin-parent pin-parent)
		  (if (and (eq 'e/pin:pin (type-of sym-or-pin))
			   (e/pin::output-p sym-or-pin))
		      sym-or-pin
		      (error (format nil "input pin given as ~S but must be a symbol, string or an output pin" sym-or-pin)))))
	  lis))

(defun new-code (&key (name "") (input-pins nil) (output-pins nil))
  (let ((self (make-instance 'code :name name)))
    (let ((inpins (make-in-pins self input-pins))
	  (opins  (make-out-pins self output-pins)))
    (setf (e/part:namespace-input-pins self) inpins)
    (setf (e/part:namespace-output-pins self) opins)
    self)))

  
(defmethod name ((p part))
  (if (string= "" (debug-name p))
      p
    (debug-name p)))

(defgeneric busy-p (self))

(defmethod busy-p ((self code))
  (busy-flag self))

(defmacro with-atomic-action (&body body)
  ;; basically a no-op in this, CALL-RETURN (non-asynch) version of the code
  ;; this matters only when running in a true interrupting environment (e.g. bare hardware, no O/S)
  `(progn ,@body))

(defmethod busy-p ((self schematic))
  (with-atomic-action
   (or (e/part:busy-flag self) ;; never practically true in this implementation (based on CALL-RETURN instead of true interrupts)
       (some #'has-input-queue-p (internal-parts self))
       (some #'has-output-queue-p (internal-parts self))
       (some #'busy-p (internal-parts self)))))

(defmethod has-input-queue-p ((self part))
  (not (null (input-queue self))))

(defmethod has-output-queue-p ((self part))
  (not (null (output-queue self))))

(defun must-find-name-in-namespace (namespace sym)
  (mapc #'(lambda (pin)
	    (when (if (symbolp sym)
                      (eq sym (e/pin:pin-name pin))
                    (string= sym (e/pin:pin-name pin)))
	      (return-from must-find-name-in-namespace pin)))
	namespace)
  (error (format nil "Name ~S not found in namespace ~S" sym namespace)))

(defmethod get-input-pin ((self part) pin-sym)
  (must-find-name-in-namespace (namespace-input-pins self) pin-sym))

(defmethod get-output-pin ((self part) pin-sym)
  (must-find-name-in-namespace (namespace-output-pins self) pin-sym))

;; part api

(defmethod ready-p ((self part))
  (and (input-queue self)
       (not (busy-p self))))

(defmethod exec1 ((self part))
  ;; execute exactly one input event to completion, then RETURN
  (let ((event (pop (input-queue self))))
    (setf (busy-flag self) t)
    (funcall (input-handler self) self event)
    (setf (busy-flag self) nil)))

(defmethod output-queue-as-list-and-delete ((self part))
  ;; return output queue as a list of output events,
  ;; null out the output queue
  (let ((list (output-queue self)))
    (setf (output-queue self) nil)
    list))

