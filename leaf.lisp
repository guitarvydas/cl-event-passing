(in-package :e/leaf)

(defclass leaf (part)
  ())

(defun make-leaf (&key (first-time nil)
                       (reactor nil)
                       (in-pins  nil)
                       (out-pins nil))
  (let ((ins (or in-pins (e/pin-bag:make-empty-bag)))
        (outs (or in-pins (e/pin-bag:make-empty-bag))))
    (make-instance 'leaf
                   :first-time first-time
                   :reactor reactor
                   :in-pins ins
                   :out-pins outs)))
