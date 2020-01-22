(in-package :e/dispatch)

(defparameter *all-parts* nil)

(defparameter *dispatcher-running* nil)

(defun reset ()
  (setf *all-parts* nil)
  (setf *dispatcher-running* nil))

(defun all-parts-have-empty-input-queues-p ()
  (dolist (part *all-parts*)
    (when (e/part::input-queue part)
      (return-from all-parts-have-empty-input-queues-p nil)))
  t)

(defun all-parts-have-empty-output-queues-p ()
  (dolist (part *all-parts*)
    (when (e/part::output-queue part)
      (return-from all-parts-have-empty-output-queues-p nil)))
  t)

(defmethod memo-part ((part e/part:part))
  (unless (e/util::in-list-p *all-parts* part #'equal)
    (push part *all-parts*)))

(defun dispatch-single-input ()
  (dolist (part *all-parts*)
    (when (e/part::ready-p part)
      (e/part::exec1 part)
      (return-from dispatch-single-input part)))
  (assert nil)) ;; can't happen

(defun dispatch-output-queues ()
  (@:loop
    (@:exit-when (all-parts-have-empty-output-queues-p))
    (dispatch-some-output-queues)))

(defun dispatch-some-output-queues()
  (dolist (part *all-parts*)
    (when (e/part::has-output-p part)
      (let ((out-list (e/part::output-queue-as-list-and-delete part)))
        (setf state :keep-looping)
        (dolist (out-event out-list) ;; for every output event...
          (if (e/part::has-parent-p part)    ;;   if part has a parent schematic, get the associated Source
              (let ((source (e/schematic::lookup-source-in-parent (e/part:parent-schem part) part out-event)))
                (when source ;; source is NIL if the pin is N.C. (no connection)
                  (e/util::trace)
                  (e/source::source-event source out-event)))
            ;; else this is the top-level part (no parent schem), so just printf the event data to stdout
            (format *standard-output* "~&~S" (e/event:data out-event))))))))

(defun run-first-times ()
  (dolist (part *all-parts*)
    (e/part::first-time part)))

(defun run ()
  (setf *dispatcher-running* t)
  (@:loop
   (e/dispatch::dispatch-output-queues)
   (@:exit-when (e/dispatch::all-parts-have-empty-input-queues-p))
   (e/dispatch::dispatch-single-input))
  (setf *dispatcher-running* nil))

;; api
(defun start-dispatcher ()
  (unless *dispatcher-running*
    (run-first-times)
    (run)))

(defun dispatcher-active-p ()
  *dispatcher-running*)

(defun ensure-correct-number-of-parts (n)
  (unless (= n (length *all-parts*))
    (error (format nil "expected ~a parts, got ~a" n (length *all-parts*)))))