(in-package :cl-event-passing-user)

;; reuse existing part test1 inside of a new schematic top-schem

(defun test-reuse1 ()
  (@initialize)

  (let ((top-schem (@new-schematic :name "top-schem" :output-pins '(:top-schem-out)))
        (child (@reuse-part cl-event-passing-user::*test1-net*
                            :name "reused test1" :output-pins '(:schem-out)))
        (wire2 (@new-wire :name "wire200")))

    (@top-level-schematic top-schem)

    (@add-part-to-schematic top-schem child)

    (@add-receiver-to-wire wire200 (e/part::get-output-pin top-schem :top-schem-out))

    (@start-dispatcher)))

  
(defun cl-user::rtest()
  ;; convenience function - to be deleted
  (cl-event-passing-user::test-reuse1))