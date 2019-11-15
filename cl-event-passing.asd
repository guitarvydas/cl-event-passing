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
                                     (:file "part" :depends-on ("package"))
                                     (:file "schematic" :depends-on ("package" "part" "source" "event"))
                                     (:file "event" :depends-on ("package"))
                                     (:file "source" :depends-on ("package" "event" "wire"))
                                     (:file "receiver" :depends-on ("package" "event" "part"))
                                     (:file "wire" :depends-on ("package" "util" "receiver" ))
                                     (:file "dispatch" :depends-on ("package" "util" "part" "event"))
				     (:file "api"  :depends-on ("package" "util" "part" "schematic" "event"
                                                                "source" "receiver" "wire" "dispatch"))
				     (:file "test0" :depends-on ("api"))
				     (:file "test6" :depends-on ("api"))))))
