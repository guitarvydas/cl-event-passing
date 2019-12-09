(in-package :cl-event-passing-user)

(defparameter *top-level-part* nil)

;; internal method (not API)
(defmethod is-top-level-p ((part e/part:part))
  (assert *top-level-part*)
  (eq part *top-level-part*))


;; API...

(defun @new-schematic (&key (name "") (input-pins nil) (output-pins nil) (first-time-handler nil))
  (let ((schem (e/schematic::new-schematic :name name :input-pins input-pins :output-pins output-pins
                                           :first-time-handler first-time-handler)))
    schem))

(defun @new-code (&key (name "") (input-pins nil) (output-pins nil) (input-handler nil) (first-time-handler nil))
  (let ((part (e/part::new-code :name name)))
    (setf (e/part:namespace-input-pins part) (e/part::make-in-pins part input-pins))
    (setf (e/part:namespace-output-pins part) (e/part::make-out-pins part output-pins))
    (setf (e/part:input-handler part) input-handler)
    (setf (e/part:first-time-handler part) first-time-handler)
    part))

(defun @reuse-part (proto &key (name "") (input-pins nil) (output-pins nil))
  (let ((cloned-part (e/part::reuse-part proto :name name)))
    (e/part::ensure-congruent-input-pins cloned-part input-pins)
    (e/part::ensure-congruent-output-pins cloned-part output-pins)
    cloned-part))

(defun @new-wire (&key (name ""))
  (e/wire::new-wire :name name))

(defun @new-event (&key (event-pin nil) (data nil))
  (e/event::new-event :event-pin event-pin :data data))

(defun @initialize ()
  (e/util::reset)
  (e/dispatch::reset))

(defmethod @top-level-schematic ((schem e/part:schematic))
  (setf *top-level-part* schem)  ;; for debugging only
  (e/dispatch::memo-part schem)) ;; always needed

(defmethod @set-first-time-handler ((part e/part:part) fn)
  (setf (e/part::first-time-handler part) fn))

(defmethod @set-input-handler ((part e/part:part) fn)
  (setf (e/part::input-handler part) fn))

(defmethod @add-receiver-to-wire ((wire e/wire:wire) (pin e/pin:pin))
  (let ((rcv (e/receiver::new-receiver :pin pin)))
    (e/wire::ensure-receiver-not-already-on-wire wire rcv)
    (e/wire::add-receiver wire rcv)))

(defmethod @add-source-to-schematic ((schem e/part:schematic) (pin e/pin:pin) (wire e/wire:wire))
  (let ((s (e/source::new-source :pin pin :wire wire)))
    (e/schematic::ensure-source-not-already-present schem s)
    (e/schematic::add-source schem s)))

(defmethod @add-part-to-schematic ((schem e/part:schematic) (part e/part:part))
  (e/schematic::ensure-part-not-already-present schem part)
  (e/schematic::add-part schem part)
  (e/dispatch::memo-part part))

(defmacro @with-dispatch (&body body)
  `(flet ((@inject (part pin data)
            (unless (eq 'e/pin:input-pin (type-of pin))
              (error "pin must be specified with get-pin (~s)" pin))
            (let ((e (e/event::new-event :event-pin pin :data data)))
              (e/util:logging e)
              (push e (e/part:input-queue part)))))
     (e/dispatch::run-first-times)
     ,@body
     (e/dispatch::run))) ;; this might create huge input queues ; maybe we want @with-dispatch-loop where the body contains no loops

(defmethod @send ((self e/part:part) (sym SYMBOL) data)
  (@send self (e/part::get-output-pin self sym) data))

(defmethod @send ((self e/part:part) (pin e/pin:pin) data)
  (let ((e (e/event:new-event :event-pin pin :data data)))
    (e/util:logging e)
    (push e (e/part:output-queue self))))

(defun @start-dispatcher ()
  (e/dispatch::start-dispatcher))

(defun @history ()
  (e/util::get-logging))

(defun @enable-logging ()
  (e/util::enable-logging))

(defmethod @get-pin ((self e/part:part) (e e/event:event))
  ;; return symbol for input pin of e
  (declare (ignore self))
  (e/part::ensure-valid-input-pin self e)
  (e/event::sym e))

(defmethod @get-data ((self e/part:part) (e e/event:event))
  ;; return data from event
  (declare (ignore self))
  (e/event::data e))

(defmethod @get-instance-var ((self e/part:part) name)
  (e/part::get-instance-var self name))

(defmethod @set-instance-var ((self e/part:part) name val)
  (e/part::set-instance-var self name val))


