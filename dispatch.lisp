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
  (dolist (part *all-parts*)
    (when (e/part:output-queue part)
      (let ((out-list (e/part::output-queue-as-list-and-delete part)))
        (dolist (out-event out-list)
          (if (cl-event-passing-user::is-top-level-p part)
              (progn
                (e/util::logging "output to console")
                (print (e/event:data out-event) *standard-output*))
              #+nil(format *standard-output* "~&part ~S outputs ~S on pin ~S~%"
                      (e/part::name part) (e/event:data out-event) (e/event:pin out-event))
            (let ((source (e/schematic::lookup-source-in-parent (e/part:parent-schem part) part out-event)))
              (when source ;; nil if NC
                (e/source::deliver-event source out-event)
                (dispatch-output-queues))))))))) ;; if any part had an output, start all over again (this is loop - tail recursive?)


(defun run-first-times ()
  (dolist (part *all-parts*)
    (let ((fn (e/part:first-time-handler part)))
      (when fn
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