(in-package :e/wire)

;; a Wire is a list of Receivers

(defclass wire ()
  ((receivers :accessor receivers :initform nil)
   (debug-name :accessor debug-name :initarg :name)))

(defun new-wire (&key (name ""))
  (make-instance 'wire :name name))

(defmethod deliver-event ((wire wire) (e e/event:event))
  (e/util:logging wire)
  (mapc #'(lambda (recv)
            (e/receiver::deliver-event recv e))
        (receivers wire)))

(defmethod ensure-receiver-not-already-on-wire ((wire wire) (rcv e/receiver:receiver))
  (e/util:ensure-not-in-list (receivers wire) rcv #'e/receiver::receiver-equal
                             "receiver ~S already on wire ~S" rcv wire))

(defmethod add-receiver ((wire wire) (rcv e/receiver:receiver))
  (push rcv (receivers wire)))

(defmethod set-receiver-list ((wire wire) lis)
  (setf (receivers wire) lis))

(defmethod name ((w wire))
  (if (stringp (debug-name w))
      (if (not (string= "" (debug-name w)))
          (debug-name w)
        w)
    (if (numberp (debug-name w))
        (debug-name w)
      w)))
