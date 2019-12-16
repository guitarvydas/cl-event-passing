(in-package :e/util)

(defparameter *logging* nil)
(defparameter *log-inputs* nil)
(defparameter *log-outputs* nil)
(defparameter *log-parts* :all)

(defun enable-logging (n)
  (setf *log-inputs* t)
  (setf *log-outputs* t)
  (if (> n 1)
      (setf *logging* t)))

(defun log-part (instance)
  (when (eq :all *log-parts*)
      (setf *log-parts* nil))
  (push instance *log-parts*))

(defmethod log-all-parts ((self e/part:schematic))
  (dolist (p (e/part::internal-parts self))
    (log-part p)))

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
  (when (or *logging* *log-inputs* *log-outputs*)
    (list *logs* *sent-events*)))

(defmethod loggable ((self e/part:part))
  (or (eq :all *log-parts*)
      (member self *log-parts*)))

(defmethod log-input ((self e/part:part) (e e/event:event))
  (when (and *log-inputs* (loggable self))
    (push
     (format nil "part ~a input ~a" (e/part::name self) (e/event::sym e))
     *logs*)))

(defmethod log-outputs ((self e/part:part))
  (when (and *log-outputs* (loggable self))
    (dolist (out-event (e/part::output-queue-as-list self))
      (push
       (format nil "part ~a output ~a" (e/part::name self) (e/event::sym out-event))
       *logs*))))

