(in-package :e/util)

(defparameter *logging* nil)

(defun enable-logging ()
  (setf *logging* t))

(defmethod logging ((e e/event:event))
  (when *logging*
    (push
     (format nil "event pin=~a data=~S" (e/event::sym e) (e/event::data e))
     *sent-events*)))

(defmethod logging ((w e/wire:wire))
  (when *logging*
    (push
     (format nil "wire ~a" (e/wire::name w))
     *sent-events*)))

(defmethod logging ((p e/part:part))
  (when *logging*
    (multiple-value-bind (state sucxess)
        (e/part::get-instance-var p :state)
      (let ((name (e/part::name p)))
        (let ((log-message (if success
                               (format nil "Part ~a state ~a" name state)
                             (format nil "Part ~a" name))))
          (push log-message *sent-events*))))))

(defmethod logging ((other T))
  (when *logging*
    (push other *sent-events*)))

(defun get-logging ()
  (when *logging*
    *sent-events*))

