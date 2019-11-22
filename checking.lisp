(in-package :e/util)

;; error checks - debug only while we need to assemble diagrams by hand

(defmethod check-top-level-schematic-sanity ((self e/schematic:schematic))
  (check-part-sanity self))

(defmethod check-top-level-schematic-sanity ((self e/part:code))
  (error (format nil "top level part must be a schematic, but is a code part ~S" self)))

(defmethod check-part-sanity ((self e/part:code))
  (unless (e/part::input-handler self)
    (error (format nil "part ~S must have an input handler" self))))

(defmethod check-part-sanity ((self e/schematic:schematic))
  (e/schematic::ensure-source-sanity self))



        