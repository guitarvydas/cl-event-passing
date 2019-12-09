(in-package :e/util)

(defparameter *logging* nil)

(defun enable-logging ()
  (setf *logging* t))

(defmethod logging ((e e/event:event) &optional (message nil))
  (when *logging*
    (push
     (format nil "event pin=~a data=~S" (e/event::sym e) (e/event::data e))
     *sent-events*)))

(defmethod logging ((w e/wire:wire) &optional (message nil))
  (when *logging*
    (push
     (format nil "wire ~a" (e/wire::name w))
     *sent-events*)))

(defmethod logging ((p e/part:part) &optional (message nil))
  (when *logging*
    (multiple-value-bind (state success)
        (e/part::get-instance-var p :state)
      (let ((name (e/part::name p))
            (m (or message "")))
        (let ((log-message (if success
                               (format nil "~a part ~a state ~a" m name state)
                             (format nil "~a part ~a" m name))))
          (push log-message *sent-events*))))))

(defmethod logging ((other T) &optional (message nil))
  (when *logging*
    (push other *sent-events*)))

(defun get-logging ()
  (when *logging*
    *sent-events*))

