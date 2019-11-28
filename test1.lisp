(in-package :cl-event-passing-user)

(defun test1 ()
  (@initialize)

  (let ((schem (@new-schematic :name "schem" :output-pins '(:schem-out)))
        (producer (@new-code :name "producer" :output-pins '(:producer-out)))
        (consumer (@new-code :name "consumer" :input-pins '(:in) :output-pins '(:consumer-out)))
        (wire1 (@new-wire :name "wire1"))
        (wire2 (@new-wire :name "wire2")))

    (@top-level-schematic schem)

    (@add-part-to-schematic schem producer)
    (@add-part-to-schematic schem consumer)

    (@set-first-time-handler producer #'test1-produce-first-time)
    (@set-input-handler consumer #'test1-consume-and-forward)

    (@add-receiver-to-wire wire1 (e/part::get-input-pin consumer :in))
    (@add-receiver-to-wire wire2 (e/part::get-output-pin schem :schem-out))
    (let ((pin (e/part::get-output-pin producer :producer-out)))
      (@add-source-to-schematic schem pin  wire1))
    (let ((pin (e/part::get-output-pin consumer :consumer-out)))
      (@add-source-to-schematic schem pin wire2))

    (@start-dispatcher)))

(defmethod test1-produce-first-time ((self e/part:part))
  (@send self (e/part::get-output-pin self :producer-out) "hello from test 1"))

(defmethod test1-consume-and-forward ((self e/part:part) (e e/event:event))
  (@send self (e/part::get-output-pin self :consumer-out) (e/event:data e)))

  
