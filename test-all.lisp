(in-package :cl-user)

(defun test-all ()
  (format *standard-output* "~&test0~%")
  (cl-event-passing-user::test0)
  (format *standard-output* "~&test6~%")
  (cl-event-passing-user::test6)
  (format *standard-output* "~&test6a~%")
  (cl-event-passing-user::test6a)
  (format *standard-output* "~&test7~%")
  (cl-event-passing-user::test7)
  (format *standard-output* "~&test8~%")
  (cl-event-passing-user::test8)
  (format *standard-output* "~&test9~%")
  (cl-event-passing-user::test9)
  (format *standard-output* "~&test 16~%")
  (cl-event-passing-user::test16))