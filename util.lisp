(in-package :e/util)

(defun ensure-not-in-list (list object equality-test fmt-msg &rest fmt-args)
  (mapc #'(lambda (item)
            (let ((exists (funcall equality-test item object)))
              (when exists
                (error (apply #'cl:format fmt-msg fmt-args)))))
        list))