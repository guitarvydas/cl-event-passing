(in-package :cl-event-passing-user)

(defun test-reuse ()
  (@initialize)

  (let ((schem (@new-schematic :name "reuse schem" :input-pins '(:in) :output-pins '(:out)))
        (child (@reuse-part cl-event-passing-user::*test6-net*
                            :name "reused test6" :input-pins '(:main-schem-in) :output-pins '(:main-schem-out)))
        (wire-main-to-child (@new-wire :name "wire main to child"))
        (wire-child-to-main (@new-wire :name "wire child to main")))

    (@top-level-schematic schem)

    (@add-part-to-schematic schem child)

    ;; wires that go INTO parts
    (@add-receiver-to-wire  wire-main-to-child         ;; wire
                            (e/part::get-input-pin child :main-schem-in)) ;; part / pin
    ;; wire that go OUT OF parts
    (@add-receiver-to-wire  wire-child-to-main
                            (e/part::get-output-pin schem :out))

    (@add-source-to-schematic schem (e/part::get-input-pin schem :in) wire-main-to-child)
    (@add-source-to-schematic schem (e/part::get-output-pin child :main-schem-out) wire-child-to-main)

    (@send schem (e/part::get-input-pin schem :in) "reuse test")))
