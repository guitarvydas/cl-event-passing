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
  (e/util:ensure-not-in-list *all-parts* part #'equal
                             "part ~S already on dispatcher list" (e/part::name part))
  (push part *all-parts*))

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
                  (e/source::source-event source out-event)))
            ;; else this is the top-level part (no parent schem), so just printf the event data to stdout
            (format *standard-output* "~S" (e/event:data out-event))))))))

(defun run-first-times ()
  (dolist (part *all-parts*)
    (let ((fn (e/part:first-time-handler part)))
      (format *standard-output* "~&checking first time of ~S ~S~%" part fn)
      (when fn
        (format *standard-output* "~&calling first time of ~S~%" part)
        (funcall fn part)))))

(defun run ()
  (@:loop
   (e/dispatch::dispatch-output-queues)
   (@:exit-when (e/dispatch::all-parts-have-empty-input-queues-p))
   (e/dispatch::dispatch-single-input)))

;; api
(defun start-dispatcher ()
  (unless *dispatcher-running*
    (setf *dispatcher-running* t)
    (run-first-times)
    (run)
    (setf *dispatcher-running* nil)))

(defun dispatcher-active-p ()
  *dispatcher-running*)