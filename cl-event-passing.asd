(defsystem "cl-event-passing"
  :depends-on (loops)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "util" :depends-on ("package"))
                                     (:file "pin" :depends-on ("package"))
                                     (:file "part" :depends-on ("package" "pin"))
                                     (:file "schematic" :depends-on ("package" "pin" "part" "source" "event"))
                                     (:file "event" :depends-on ("package"))
                                     (:file "source" :depends-on ("package" "pin" "event" "wire"))
                                     (:file "receiver" :depends-on ("package" "pin" "event" "part"))
                                     (:file "wire" :depends-on ("package" "util" "receiver" ))
                                     (:file "dispatch" :depends-on ("package" "util" "part" "event"))
				     (:file "api"  :depends-on ("package" "util" "pin" "part" "schematic" "event"
                                                                "source" "receiver" "wire" "dispatch"))
				     (:file "macro-support"  :depends-on ("package" "util" "pin" "part" "schematic" "event"
                                                                          "source" "receiver" "wire" "dispatch"))
				     (:file "macro"  :depends-on ("package" "util" "pin" "part" "schematic" "event"
                                                                          "source" "receiver" "wire" "dispatch"))
				     (:file "test0" :depends-on ("api"))
				     (:file "test6" :depends-on ("api"))
				     (:file "test6a" :depends-on ("api"))
				     (:file "test7" :depends-on ("api"))
				     (:file "test8" :depends-on ("api"))
				     (:file "test9" :depends-on ("api"))

				     (:file "test16" :depends-on ("api" "macro"))

                                     (:file "test-all" :depends-on ("api" "test0" "test6" "test6a" "test7" "test8" "test9" "test16"))))))

#|
;; wip?  future examples

(defsystem "cl-event-passing/example"
  :depends-on (cl-event-passing)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "parts"
                        :pathname "./parts/"
                        :components ((:file "file-stream")
                                     (:file "iter")
                                     (:file "cat")))
  
               (:module "example"
                        :pathname "./"
                        :components ((:file "example1")))))
|#