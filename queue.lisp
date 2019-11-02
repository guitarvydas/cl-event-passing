(in-package :e/queue)

;; fifo queue

(defclass queue ()
  ((q :accessor q :initform nil)))

(defmethod q-push ((self queue) (m e/message:message))
  (setf (q self) (append q (list m))))

(defmethod q-pop ((q queue))
  (pop q))

(defun make-queue ()
  (make-instance 'queue))

(defmethod as-list ((self queue))
  (q self))
