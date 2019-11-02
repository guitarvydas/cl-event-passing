(in-package :e/dispatch)


(defparameter *parts-list* nil)  ;; used by Dispatcher

(defun clear-parts ()
  (setf *parts-list* nil))

(defmethod remember-part ((part e/part:part))
  (push part *parts-list*))
                      

(defparameter *dispatcher-running* nil)

;; my convention - routines that begin with '@' are "high level" architectural routines, to be implemented
;; in "lower levels"

(defun @set-running () 
  (setf *dispatcher-running* t))

(defun @Dispatcher-already-running-p ()
  *dispatcher-running*)

(defun @all-parts-have-no-inputs ()
  (notany #'e/part:has-input-p *parts-list*))

(defun @get-random-ready-part ()
  (let ((chosen-part nil)
        (parts-list *parts-list*))
    (@:loop
      (@:exit-when chosen-part)
      (assert parts-list)
      (let ((part-under-consideration (pop parts-list)))
        (when (e/part:has-input-p part-under-consideration)
          (setf chosen-part part-under-consideration))))
    chosen-part))

(defmethod @release-output-queue-externally ((p e/part:part))
  (let ((oq (e/part:outqueue-as-list  p)))
    (mapc #'(lambda (msg)
              (format *standard-output* "~&pin/~S/ message/~S/~%"
                      (e/message:pin msg)
                      (e/message:data msg)))
          oq)))

#|
(defmethod rewrite-pin ((pin e/pin:pin) (message e/message:message))
  (let ((new-msg (e/message:clone-with-pin message pin)))
    new-msg))
|#

#|
(defmethod @release-message-via-wiring-list ((wiring-list e/wiring-list:wiring-list) (msg e/message:message))
  (let ((pin (e/message:pin msg)))
    (let ((wire (e/wire-list:find-wire wiring-list pin)))
      (let ((dest-list (e/wire:outs-as-list wire)))
 (eget-destinations wiring-list (pin msg))))
    
    (mapc #'(lambda (dest)
              (q-push (inqueue dest) (rewrite-pin (pin dest) msg)))
          dest-list)))
|#

(defmethod @release-output-queue-internally ((schematic e/schematic:schematic) (part e/part:part))
  (let ((out-list (e/part:outqueue-as-list part)))
    (mapc #'(lambda (message)
              (let ((out-pin (e/message:pin message))
                    (data (e/message:data message)))
                (let ((destination-wire (e/schematic:find-wire-for-pin-inside-schematic schematic part out-pin)))
                  (e/wire:deliver-message wire message))))
          out-list)))

(defmethod @run-part-with-message ((p e/part:part) (m e/message:message))
  (e/part:react p m))

(defmethod @release-output-queue ((p e/part:part))
  "using the wiring map of the parent, send every message
   to its destination part, rewriting OUTPUT pins to INPUT pins,
   if no parent, write message to stdout"
  (let ((schematic (e/part:parent p)))
    (if schematic
        (@release-output-queue-internally schematic p)
      (@release-output-queue-externally p))))

(defun @Call-First-Times ()
  (mapc #'(lambda (part)
            (when (e/part:has-first-time-p part)
              (funcall (e/part:first-time-function part))))
        *parts-list*))

(defun Start-Dispatcher ()
  (unless (@Dispatcher-already-running-p)
    (@Call-First-Times)
    (Dispatcher)))

(defun Dispatcher ()
  (@:loop
    (@set-running)
    (@:exit-when (@all-parts-have-no-inputs))
    (let ((part (@get-random-ready-part)))
      (let ((msg (e/part:pop-input part)))
        (@run-part-with-message part msg)
        (@release-output-queue part)))))


