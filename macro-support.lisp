(in-package :cl-event-passing)

;; functions that help the defnetwork macro work

(defmethod make-inbound-receiver ((part e/part:part) (pin-sym SYMBOL))
  (let ((pin (e/part::get-input-pin part pin-sym)))
    (let ((ibrcv (e/receiver::new-inbound-receiver :pin pin)))
      ibrcv)))

(defmethod make-outbound-receiver ((part e/schematic:schematic) (pin-sym SYMBOL))
  (let ((pin (e/part::get-output-pin part pin-sym)))
    (let ((obrcv (e/receiver::new-outbound-receiver :pin pin)))
      obrcv)))

(defmethod make-wire (receiver-list)
  (let ((w (cl-event-passing-user:@new-wire :name (e/util::get-wire-number))))
    (e/wire::set-receiver-list w receiver-list)
    w))

(defmethod make-source-coming-from-outside ((schem e/schematic:schematic) sym)
  (let ((pin (e/part::get-input-pin schem sym)))
    (e/source::new-source :pin pin :wire nil))) ;; wire is temporarily nil, will be set later


(defmethod make-source-from-child ((part e/part:part) sym)
  (let ((schem (e/part:parent-schem part)))
    (let ((pin (e/part::get-output-pin part sym)))
      (e/source::new-source :pin pin :wire nil)))) ;; wire is temporarily nil, will be set later

(defmethod make-sources-for-wire ((self e/schematic:schematic) (wire e/wire:wire) list-of-sources)
  (mapc #'(lambda (s)
            (e/source::set-wire-of-source s wire)
            (e/schematic::add-source-with-ensure self s))
        list-of-sources))

