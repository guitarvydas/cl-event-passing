(in-package :e/api)

(defgeneric @send (self e))

(defgeneric @add-receiver-to-wire (self part pin))

(defgeneric @busy-p (self))

(defgeneric @set-input-handler (self func))

(defgeneric @set-output-handler (self func))

(defgeneric @set-first-time-handler (self))

(defgeneric @start-dispatcher ())
