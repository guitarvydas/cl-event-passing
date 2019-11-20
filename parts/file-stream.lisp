;;INPUTS
;; (:file-name "string") opens file and sends stream to :stream
;; (:read  T)            read one item from file, send it to :object
;; (:close T)            close file, removes internal memory of *stream*
;;
;;OUTPUTS
;; (:stream stream)      the Lisp stream
;; (:object T)           a Lisp object
;; (:done T)             EOF, send :FATAL if receive anything but :file-name
;; (:fatal object)       some fatal error, "object" specifies error details (NIY (not implemented yet))


(in-package :cl-event-passing)

(defparameter *stream* nil)

(defmethod file-stream ((self e/part:part) (e e/event:event))
  (let ((data (e/event:data e))
        (action (e/event:pin e))
        (state 'idle))

    (case state

      (idle
           (case action
             (:file-name (let ((in (open data :direction :input)))
                           (setf *stream* in)
                           (cl-event-passing-user:@send self :stream in)
                           (setf state 'open))
             (otherwise  (cl-event-passing-user:@send self :fatal t)))))

      (open
           (case action
             (:read (let ((object (read *stream* nil 'EOF)))
                      (if (eq object 'EOF)
                          (cl-event-passing-user:@send self :done t) ;; nb - don't close, send message to self
                        (cl-event-passing-user:@send self :object object))))

             (:close (setf *stream* nil)
              (setf state 'idle))

             (otherwise (cl-event-passing-user:@send self :fatal t))))
      
       (otherwise (cl-event-passing-user:@send self :fatal t)))))

