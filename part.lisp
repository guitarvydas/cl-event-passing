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

(defun clone-part (self proto)
  (setf (input-queue self) nil)
  (setf (output-queue self) nil)
  (setf (busy-flag self) nil)
  (setf (first-time-handler self) (first-time-handler proto))
  (setf (input-handler self) (input-handler proto))
  (setf  (debug-name self) (format nil "cloned ~S" (debug-name proto)))
  ; (setf parent-schem ... fixed up later
  (setf (namespace-input-pins self) (mapcar #'(lambda (pin)
                                                (e/pin::clone-with-parent self pin))
                                            (namespace-input-pins proto)))
  (setf (namespace-output-pins self) (mapcar #'(lambda (pin)
                                                (e/pin::clone-with-parent self pin))
                                             (namespace-output-pins proto)))
  self)
 
(defmethod clone ((proto code))
  (let ((new (make-instance 'code)))
    (let ((cloned (clone-part new proto)))
      cloned)))

(defmethod clone ((proto schematic))
  (let ((new (make-instance 'schematic)))
    (let ((cloned (clone-part new proto)))
      (setf (internal-parts cloned) (mapcar #'(lambda (p)
                                             (let ((new-part (clone p)))
                                               (setf (parent-schem new-part) cloned)
                                               new-part))
                                         (internal-parts proto)))
      ;; sources must be cloned after internal-parts has been cloned, sources and wires refer to self or to internal-parts
      (setf (sources cloned) (mapcar #'(lambda (s)
                                      (e/source::clone-with-parent cloned s))
                                  (sources proto)))
      cloned)))
  
(defmethod make-in-pins ((pin-parent part) lis)
  (flet ((symbol-or-string-p (s)
           (or
            (symbolp s)
            (stringp s)))
         (already-an-input-pin-p (s)
           (and
            (eq 'e/pin:pin (type-of s))
            (e/pin::input-p s))))
    (mapcar #'(lambda (s)
                (if (symbol-or-string-p s)
                    (e/pin::new-pin :pin-name s :direction :input :pin-parent pin-parent)
                  (if (already-an-input-pin-p s)
                      s
                    (error (format nil "input pin given as ~S but must be a symbol, string or an input pin" s)))))
            lis)))

(defmethod make-out-pins ((pin-parent part) lis)
  (flet ((symbol-or-string-p (s)
           (or
            (symbolp s)
            (stringp s)))
         (already-an-output-pin-p (s)
           (and (eq 'e/pin:pin (type-of s))
                (e/pin::output-p s))))
    (mapcar #'(lambda (s)
                (if (symbol-or-string-p s)
                    (e/pin::new-pin :pin-name s :direction :output :pin-parent pin-parent)
                  (if (already-an-output-pin-p s)
                      s
                    (error (format nil "output pin given as ~S but must be a symbol, string or an output pin" s)))))
            lis)))

(defun new-code (&key (name "") (input-pins nil) (output-pins nil))
  (let ((self (make-instance 'code :name name)))
    (let ((inpins (make-in-pins self input-pins))
	  (opins  (make-out-pins self output-pins)))
    (setf (e/part:namespace-input-pins self) inpins)
    (setf (e/part:namespace-output-pins self) opins)
    self)))

(defun reuse-part (proto &key (name ""))
  (clone proto))
  
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

(defun find-name-in-namespace (namespace sym)
  (mapc #'(lambda (pin)
	    (when (if (symbolp sym)
                      (eq sym (e/pin:pin-name pin))
                    (string= sym (e/pin:pin-name pin)))
	      (return-from find-name-in-namespace pin)))
	namespace)
  nil)

(defun must-find-name-in-namespace (namespace sym)
  (let ((pin (find-name-in-namespace namespace sym)))
    (unless pin
      (error (format nil "Name ~S not found in namespace ~S" sym namespace)))
    pin))

(defun ensure-congruent-in-namespace (self namespace pin-list)
  (mapc #'(lambda (pin-sym)
            (unless (find-name-in-namespace namespace pin-sym)
              (error (format nil "pin ~S not found in ~S of part ~S" pin-sym namespace (name self)))))
        pin-list))

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

(defmethod name ((p part))
  (if (string= "" (debug-name p))
      p
    (debug-name p)))

(defmethod input-pins ((self part))
  (namespace-input-pins self))

(defmethod output-pins ((self part))
  (namespace-output-pins self))

(defmethod ensure-congruent-input-pins ((self part) input-pins)
  (ensure-congruent-in-namespace self (namespace-input-pins self) input-pins))

(defmethod ensure-congruent-output-pins ((self part) output-pins)
  (ensure-congruent-in-namespace self (namespace-output-pins self) output-pins))

(defmethod has-parent-p ((self part))
  (not (null (parent-schem self))))

(defmethod has-output-p ((self part))
  (not (null (output-queue self))))

