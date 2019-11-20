;;INPUTS
;; (:begin T)            begin iterating, once for each :next input
;; (:next  T)            send t to :next
;; (:end   T)            end iteration (send nothing)

;; 
;;OUTPUTS
;; (:next t)             request next iteration
;; (:fatal details)      fatal error (NIY)
;;

(in-package :cl-event-passing)

(defparameter *stream* nil)

(defmethod file-stream ((self e/part:part) (e e/event:event))
  (let ((data (e/event:data e))
        (action (e/event:pin e))
        (setf state 'idle))

    (case state

      (idle
           (case action
             (:begin (setf state 'running)
             (otherwise  (cl-event-passing-user:@send self :fatal t)))))

      (running
           (case action
             (:next (cl-event-passing-user:@send self :next t))

             (:end (setf state 'idle))
             
             (otherwise (cl-event-passing-user:@send self :fatal t))))
      
       (otherwise (cl-event-passing-user:@send self :fatal t)))))

