(in-package :e/wire)

;; a Wire is a list of Receivers

(defclass wire ()
  ((receivers :accessor receivers :initform nil)
   (debug-name :accessor debug-name :initarg :name :initform "")))

(defun new-wire (&key (name ""))
  (make-instance 'wire :name name))

(defmethod clone-with-parent ((cloned-parent e/part:part) (proto wire))
  (let ((new (make-instance 'wire :name (debug-name proto))))
    (setf (debug-name new) (format nil "cloned wire ~S" (debug-name proto)))
    (setf (receivers new) (mapcar #'(lambda (proto-receiver)
                                      (e/receiver::clone-with-parent cloned-parent proto-receiver))
                                  (receivers proto)))
    new))

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

(defmethod ensure-wire-sanity (schem (self wire))
  (mapc #'(lambda (r)
            (e/receiver::ensure-receiver-sanity schem r))
        (receivers self)))