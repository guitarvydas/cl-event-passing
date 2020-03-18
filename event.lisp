(in-package :e/event)

(defclass event ()
  ((event-pin :accessor event-pin :initarg :event-pin)
   (data :accessor data :initarg :data)
   (tag :accessor tag :initarg :tag :initform nil)
   (detail :accessor detail :initarg :detail :initform :tag))) ;; :none, :tag, :pin, :data (= tag+pin+data)

(defun new-event (&key (event-pin nil) (data nil) (tag nil) (detail :tag))
  (make-instance 'event :event-pin event-pin :data data :tag tag :detail detail))

(defmethod sym ((e e/event:event))
  (e/pin::pin-name (event-pin e)))

(defmethod display-event (self (event e/event:event) direction)
  (ecase (detail event)
    (:none )
    (:tag
     (when (e/event:tag event)
       (format *error-output* "~&~a ~s [~s]~%" direction self (e/event:tag event))))
    (:pin
     (when (e/event:tag event)
       (format *error-output* "~&~a ~s [~s][~s]~%" direction self (e/event:tag event) (e/event:event-pin event))))
    (:data
     (when (e/event:tag event)
       (format *error-output* "~&~a ~s [~s][~s][~s]~%" 
               direction
               self
               (e/event:tag event)
               (e/event:event-pin event)
               (e/event:data event))))))

(defmethod tag-merge ((e1 event) (e2 event))
  (cond ((null (tag e1)) (tag e2))
        (t (cons (tag e1) (tag e2)))))

(defmethod detail-merge ((e1 event) (e2 event))
  (cond ((eq :none (detail e1)) (detail e2))
        ((eq :data (detail e1)) :data)
        ((eq :pin (detail e1))
         (if (eq :data (detail e2))
             :data
           :pin))
        (t :tag)))

(defmethod display-and-propogate-event (self (prototype-e e/event:event) (e e/event:event) direction)
  (setf (tag e) (tag-merge e prototype-e))
  (setf (detail e) (detail-merge e prototype-e))
  (display-event self e direction))

(defmethod display-output-events (self (input-e e/event:event) e-list)
  (when (tag input-e)
    (dolist (e e-list)
      (display-and-propogate-event self input-e e "output"))))



