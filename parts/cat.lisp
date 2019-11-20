;;INPUTS
;; (:in object) @send object to :out
;;
;;OUTPUTs
;; :out   the lisp object
;; :fatal some fatal error message (NIY)

(in-package :cl-event-passing-user)

(defmethod cat ((self e/part:part) (e e/event:event))
  (let ((lisp-obj (e/event:data e))
        (action (e/event:pin e)))
    (case action
      (:in (cl-event-passing-user:@send self :out lisp-obj))
      (otherwise (cl-event-passing-user:@send self :fatal t)))))
