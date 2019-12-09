(in-package :e/util)

(defparameter *logging* nil)

(defun enable-logging ()
  (setf *logging* t))

(defmethod logging ((e e/event:event))
  (when *logging*
    (push
     (format nil "event pin=~S data=~S" (e/event::sym e) (e/event::data e))
     *sent-events*)))

(defmethod logging ((w e/wire:wire))
  (when *logging*
    (push
     (format nil "wire ~S" (e/wire::name w))
     *sent-events*)))

(defmethod logging ((p e/part:part))
  (when *logging*
    (push
     (format nil "part ~S" (e/part::name p))
     *sent-events*)))

(defmethod logging ((other T))
  (when *logging*
    (push other *sent-events*)))

(defun get-logging ()
  (when *logging*
    *sent-events*))

