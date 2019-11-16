(in-package :e/schematic)

(defmacro with-atomic-action (&body body)
  ;; basically a no-op in this, CALL-RETURN (non-asynch) version of the code
  ;; this matters only when running in a true interrupting environment (e.g. bare hardware, no O/S)
  `(progn ,@body))

(defclass schematic (e/part:part)
  ((sources :accessor sources :initform nil) ;; a list of Sources (which contain a list of Wires which contain a list of Receivers)
   (internal-parts :accessor internal-parts :initform nil))) ; a list of Parts

(defun new-schematic (&key (name "") (input-pins nil) (output-pins nil))
  (let ((self (make-instance 'schematic :name name)))
    (setf (e/part:namespace-input-pins self) input-pins)
    (setf (e/part:namespace-output-pins self) output-pins)
    self))

(defmethod ensure-source-not-already-present ((self schematic) (s e/source:source))
  (e/util:ensure-not-in-list (sources self) s #'equal
                             "source ~S already present in schematic ~S" s (e/part::name self)))

(defmethod add-source ((self schematic) (s e/source:source))
  (push s (sources self)))

(defmethod ensure-part-not-already-present ((self schematic) (p e/part:part))
  (e/util:ensure-not-in-list (internal-parts self) p #'equal
                             "part ~S already present in schematic ~S" p (e/part::name self)))

(defmethod add-part ((self schematic) (p e/part:part))
  (setf (e/part:parent-schem p) self)
  (push p (internal-parts self)))

(defmethod lookup-source-in-parent ((parent schematic) (self e/part:part) (e e/event:event))
  ;; find part-pin in parent's source list
  (let ((pin-sym (e/event:pin e)))
    (dolist (s (sources parent))
      (when (e/source::equal-part-pin-p s self pin-sym)
        (return-from lookup-source-in-parent s)))
    (assert nil))) ;; shouldn't happen

(defmethod lookup-source-in-self ((self schematic) (e e/event:event))
  ;; find part-pin in self's source list
  (let ((pin-sym (e/event:pin e)))
    (dolist (s (sources self))
      (when (e/source::equal-part-pin-p s self pin-sym)
        (return-from lookup-source-in-self s)))
    (assert nil))) ;; shouldn't happen

(defmethod schematic-input-handler ((self schematic) (e e/event:event))
  (let ((s (lookup-source-in-self self e)))
    (e/source::deliver-event s e)))

(defmethod busy-p ((self schematic))
  (with-atomic-action
   (or (e/part:busy-flag self) ;; never practically true in this implementation (based on CALL-RETURN instead of true interrupts)
       (some #'has-input-queue-p (internal-parts self))
       (some #'has-output-queue-p (internal-parts self))
       (some #'busy-p (internal-parts self)))))
