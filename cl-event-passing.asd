(defsystem "cl-event-passing"
  :depends-on (:loops :esrap)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "util" :depends-on ("package"))
                                     (:file "pin" :depends-on ("package" "event"))
                                     (:file "part" :depends-on ("package" "pin"))
                                     (:file "schematic" :depends-on ("package" "pin" "part" "source" "event"))
                                     (:file "event" :depends-on ("package"))
                                     (:file "source" :depends-on ("package" "pin" "event" "wire"))
                                     (:file "receiver" :depends-on ("package" "pin" "event" "part"))
                                     (:file "wire" :depends-on ("package" "util" "receiver" "pin"))
                                     (:file "dispatch" :depends-on ("package" "util" "part" "event"))
				     (:file "macro-support"  :depends-on ("package" "util" "pin" "part" "schematic" "event"
                                                                          "source" "receiver" "wire" "dispatch"))
                                     (:file "net-parser" :depends-on ("package"))
				     (:file "macro"  :depends-on ("package" "util" "pin" "part" "schematic" "event"
                                                                          "source" "receiver" "wire" "dispatch" "net-parser"))
				     (:file "api"  :depends-on ("package" "util" "pin" "part"
                                                                "schematic" "event" "source" "receiver" "wire" "dispatch"))
				     (:file "logging"  :depends-on ("api" "macro" "macro-support" "package" "util" "pin" "part"
                                                                "schematic" "event" "source" "receiver" "wire" "dispatch"))
				     (:file "checking"  :depends-on ("api" "macro" "macro-support" "package" "util" "pin" "part"
                                                                "schematic" "event" "source" "receiver" "wire" "dispatch"))))))


(defsystem "cl-event-passing/parts"
  :depends-on (cl-event-passing)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "parts"
                        :pathname "./parts/"
                        :components ((:file "fatal")
                                     (:file "filestream")
                                     (:file "iter")
                                     (:file "cat")))))

(defsystem "cl-event-passing/examples"
  :depends-on (cl-event-passing/parts)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "examples"
                        :pathname "./"
                        :components ((:file "test0")
				     (:file "test1")
				     (:file "test6")
				     (:file "test6a")
				     (:file "test7")
				     (:file "test8")
				     (:file "test9")
				     (:file "test16")
				     (:file "test26")
				     (:file "test-feedback")
				     (:file "test-reuse1" :depends-on ("test1"))
				     (:file "test-reuse" :depends-on ("test6"))
				     (:file "test-reuse3" :depends-on ("test1"))
				     (:file "test-reuse4" :depends-on ("test6"))
                                     (:file "test-all" :depends-on ("test0" "test1" "test6" "test6a" "test7" "test8" "test9"
                                                                    "test16" "test-feedback" "test-reuse"
                                                                    "test-reuse1" "test-reuse3" "test-reuse4"
                                                                    "test26"))))))


(defsystem "cl-event-passing/nesting-test"
  :depends-on (cl-event-passing)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "src"
                        :pathname "./nesting/"
                        :components ((:file "lowA")
				     (:file "capA")
				     (:file "lowB")
				     (:file "capB")
				     (:file "lowC")
				     (:file "capC")
				     (:file "lowD")
				     (:file "capD")
				     (:file "lowE")
				     (:file "capE")
				     (:file "lowF")
				     (:file "capF")

				     (:file "strcat1")
				     (:file "strcat2")
				     (:file "strcat3")
				     (:file "strcat4")
				     (:file "strcat5")

				     (:file "nesting" :depends-on ("lowA" 
								   "capA"
								   "lowB"
								   "capB"
								   "lowC"
								   "capC"
								   "lowD"
								   "capD"
								   "lowE"
								   "capE"
								   "lowF"
								   "capF"
                                                                   "strcat1"
                                                                   "strcat2"
                                                                   "strcat3"
                                                                   "strcat4"
                                                                   "strcat5"
                                                                   ))))))
									  
