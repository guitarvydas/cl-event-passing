(in-package :e/wire)

;; a wire is a collection of destination part-pin pairs (:receivers), a wire is pointed-to by any number of outputs

(defclass wire ()
  ((receivers :accessor receivers :initarg :receivers))) ;; list of part-pin pairs

