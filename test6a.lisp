(in-package :cl-event-passing-user)


;; same as test6, but with strings as pin names

(defun test6a ()
  ;; see test6.drawio tab "top level"
  (@initialize)
  ;; 4 parts, 5 wires
  (let ((main-schem (@new-schematic :name "main schem" :input-pins '("main-schem-in") :output-pins '("main-schem-out")))
        (child-schem (@new-schematic :name "child schem" :input-pins '("child-schem-in") :output-pins '("child-schem-out")))
        ;; same code part used (instantiated) twice...
        (flow-through-1 (@new-code :name "flow-through 1" :input-pins '("ft-in") :output-pins '("ft-out")))
        (flow-through-2 (@new-code :name "flow-through 2" :input-pins '("ft-in") :output-pins '("ft-out")))

        (wire-main-to-child (@new-wire :name "wire main to child"))
        (wire-child-to-flow-through-1 (@new-wire :name "wire child to flow-through-1"))
        (wire-flow-through-1-to-flow-through-2 (@new-wire :name "wire flow-through-1 to flow-through-2"))
        (wire-flow-through-2-to-child (@new-wire :name "wire flow-through-2 to child"))
        (wire-child-to-main (@new-wire :name "wire child to main")))

    (@top-level-schematic main-schem)

    (@add-part-to-schematic main-schem child-schem)

    (@add-part-to-schematic child-schem flow-through-1)
    (@add-part-to-schematic child-schem flow-through-2)

    (@set-input-handler flow-through-1 #'flow-through-6a)
    (@set-input-handler flow-through-2 #'flow-through-6a)

    ;; wires that go INTO parts
    (@add-receiver-to-wire  wire-main-to-child         ;; wire
                            (e/part::get-input-pin child-schem "child-schem-in")) ;; part / pin
    (@add-receiver-to-wire  wire-child-to-flow-through-1
                            (e/part::get-input-pin flow-through-1 "ft-in"))
    (@add-receiver-to-wire  wire-flow-through-1-to-flow-through-2
                            (e/part::get-input-pin flow-through-2 "ft-in"))

    ;; wire that go OUT OF parts
    (@add-receiver-to-wire  wire-flow-through-2-to-child
                            (e/part::get-output-pin child-schem "child-schem-out"))
    (@add-receiver-to-wire  wire-child-to-main
                            (e/part::get-output-pin main-schem "main-schem-out"))


    (@add-source-to-schematic main-schem (e/part::get-input-pin main-schem "main-schem-in") wire-main-to-child)
    (@add-source-to-schematic child-schem (e/part::get-input-pin child-schem "child-schem-in") wire-child-to-flow-through-1)
    (@add-source-to-schematic child-schem (e/part::get-output-pin flow-through-1 "ft-out") wire-flow-through-1-to-flow-through-2)
    (@add-source-to-schematic child-schem (e/part::get-output-pin flow-through-2 "ft-out") wire-flow-through-2-to-child)
    (@add-source-to-schematic main-schem (e/part::get-output-pin child-schem "child-schem-out") wire-child-to-main)

    (@with-dispatch
     (@inject main-schem (e/part::get-input-pin main-schem "main-schem-in") "test 6"))))
  
(defmethod flow-through-6a ((self e/part:part) (e e/event:event))
  (@send self (e/part::get-output-pin self "ft-out") (e/event:data e)))
