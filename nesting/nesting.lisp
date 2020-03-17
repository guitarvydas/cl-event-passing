(in-package :cl-event-passing-user)

(defun nesting ()
  (let ((net (@defnetwork nest

		(:code lowA (:in) ())
		(:code capA (:in) ())
		(:code lowB (:in) ())
		(:code capB (:in) ())
		(:code lowC (:in) ())
		(:code capC (:in) ())

		(:schem lowest (:in) ()
			(lowA capA)
			"
                        self.in -> lowA.in, capA.in
                        ")
		(:schem middle (:in) ()
			(lowB capB lowest)
			"
                        self.in -> lowest.in, lowB.in, capB.in
                        ")
		(:schem nest (:in) ()
			(lowC capC middle)
			"
                        self.in -> middle.in, lowC.in, capC.in
                        ")
		)))
    (@with-dispatch
      (let ((top-pin (e/part::get-input-pin net :in)))
        (@inject net top-pin t)))))

