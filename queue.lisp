(in-package :e/queue)

;; fifo queue

(defclass queue ()
  ((q :accessor q :initform nil)))

(defmethod q-push ((self queue) (m e/message:message))
  (setf (q self) (append (q self) (list m))))

(defmethod q-pop ((self queue))
  (pop (q self)))

(defun make-queue ()
  (make-instance 'queue))

(defmethod as-list ((self queue))
  (q self))
