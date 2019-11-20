(in-package :e/dispatch)

(defparameter *all-parts* nil)

(defun reset ()
  (setf *all-parts* nil))

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
    (when (e/part::input-queue part)
      (let ((event (pop (e/part::input-queue part))))
        (setf (e/part:busy-flag part) t)
        (funcall (e/part::input-handler part) part event)
        (setf (e/part:busy-flag part) nil)
        (return-from dispatch-single-input part))))
  (assert nil)) ;; can't happen

(defun dispatch-output-queues ()
  (dolist (part *all-parts*)
    (when (e/part:output-queue part)
      (let ((out-list (e/part:output-queue part)))
        (setf (e/part::output-queue part) nil)
        (dolist (out-event out-list)
          (if (cl-event-passing-user::is-top-level-p part)
              (print (e/event:data out-event) *standard-output*)
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
