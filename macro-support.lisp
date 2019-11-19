(in-package :cl-event-passing)

;; functions that help the defnetwork macro work

make-inbound-receiver part pin
make-outbound-receiver part pin
make-wire list-of-receivers
make-sources-for-wire wire list-of-sources
make-source-coming-from-outside schem sym
make-source-from-child part sym


(defmethod make-inbound-receiver ((part e/part:part) (pin-sym SYMBOL))
  (let ((pin (e/part::get-input-pin part pin-sym)))
