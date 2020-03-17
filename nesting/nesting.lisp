;; usage:
;; (ql:quickload :cl-event-passing/nesting-test)
;; (cl-event-passing-user::nesting)

(in-package :cl-event-passing-user)

(defun nesting ()
  (let ((net (@defnetwork nest

		(:code lowA (:in) ())
		(:code capA (:in) ())
		(:code lowB (:in) ())
		(:code capB (:in) ())
		(:code lowC (:in) ())
		(:code capC (:in) ())
		(:code lowD (:in) ())
		(:code capD (:in) ())
		(:code lowE (:in) ())
		(:code capE (:in) ())
		(:code lowF (:in) ())
		(:code capF (:in) ())

		(:schem lowest (:in) ()
			(lowA lowB lowC)
			"
                        self.in -> lowA.in, lowB.in, lowC.in
                        ")
		(:schem middle (:in) ()
			(capA capB capC lowest)
			"
                        self.in -> lowest.in,capA.in, capB.in, capC.in
                        ")
		(:schem lowest2 (:in) ()
			(lowD lowE lowF)
			"
                        self.in -> lowD.in, lowE.in, lowF.in
                        ")
		(:schem middle2 (:in) ()
			(capD capE capF lowest2)
			"
                        self.in -> lowest2.in,capD.in, capE.in, capF.in
                        ")
		(:schem nest (:in) ()
			(middle middle2)
			"
                        self.in -> middle.in,middle2.in
                        ")
		)))
    (@with-dispatch
      (let ((top-pin (e/part::get-input-pin net :in)))
        (@inject net top-pin t)))))

