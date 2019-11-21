(in-package :cl-event-passing-user)

(defun test-feedback()
  (let ((net 
         (cl-event-passing-user::@defnetwork cl-event-passing-user::main
          (:code file-stream (:file-name :read :close) (:stream :object :done :fatal)
           #'file-stream #'file-stream-reset)
          (:code iter (:begin :next :finish) (:next :fatal) #'iter #'iter-reset)
          (:code cat (:in) (:out :fatal) #'cat)
          (:code fatal (:in) () #'fatal)
          (:schem cl-event-passing-user::main (:in) (:out)
                 (file-stream iter cat fatal)
                 (
                  (((:self :in))  ((file-stream :file-name)))
                  (((iter :next)) ((file-stream :read)))
                  (((file-stream :stream)) ((iter :begin)))
                  (((file-stream :object)) ((iter :next) (cat :in)))
                  (((file-stream :done)) ((iter :finish)(file-stream :close))) ;; feedback is here
                  (((cat :out)) ((:self :out)))
                  (((file-stream :fatal) (iter :fatal) (cat :fatal)) ((fatal :in)))
                  )))))
    (let ((ap e/dispatch::*all-parts*)) ;; testing only
      (assert (= 5 (length ap)))        ;; testing only
      (@inject net
               (e/part::get-input-pin net :in) (asdf:system-relative-pathname :cl-event-passing "test-feedback.lisp"))
      (@history))))

