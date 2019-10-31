(defsystem "cl-messaging"
  :depends-on (loops)
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "queue" :depends-on ("package" "message"))
                                     (:file "pin" :depends-on ("package"))
                                     (:file "pin-bag" :depends-on ("package" "pin"))
                                     (:file "message" :depends-on ("package" "pin"))
                                     (:file "part" :depends-on ("package" "message" "pin-bag"))
                                     (:file "schematic" :depends-on ("package" "part" "wire-list" "message"))
                                     (:file "leaf" :depends-on ("package" "pin-bag"))
                                     (:file "send" :depends-on ("package" "message" "part"))
                                     (:file "receive" :depends-on ("package" "part" "message"))
                                     (:file "wire-list" :depends-on ("package" "wire" "pin"))
                                     (:file "wire" :depends-on ("package"))
                                     (:file "pin-wire" :depends-on ("package" "pin" "wire"))
                                     (:file "part-pin" :depends-on ("package" "part" "pin"))
                                     (:file "dispatch" :depends-on ("package" "part" "message"))
                                     (:file "hello-world" :depends-on ("package" "schematic" "leaf"
                                                                       "pin-bag" "pin" "part-pin" "part"
                                                                       "wire" "wire-list" "dispatch"))))))
