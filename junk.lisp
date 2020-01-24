(esrap::clear-rules)
(esrap:defrule <lisp-id-char-first> (esrap:character-ranges (#\a #\z) (#\A #\Z) #\@ #\* #\~ #\! #\- #\$ #\% #\^ #\- #\+ #\_ #\= #\{ #\} #\[ #\] #\\ #\. #\/ #\< #\> #\?))
(esrap:defrule <lisp-id-char-follow> (or (esrap:character-ranges (#\0 #\9)) <lisp-id-char-first>))
(esrap:defrule <ident> (and <lisp-id-char-first> (* <lisp-id-char-follow>)) (:text t))
