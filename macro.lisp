(in-package :cl-event-passing)

(defmacro defnetwork (&rest defs)
  `(compile-network defs))

(defun compile-network (defs)
  (if (null defs)
      nil
    (compile-one-network (first defs) (rest defs))))

(defun compile-one-network (def tail)
  (let ((one
         (case (car def)
           
           (code
            (destructuring-bind (code-name inputs outputs input-handler)
                (rest def)
              `(let ((,code-name (cl-event-passing-user:@new-code :name ,code-name :input-handler ,input-handler
                                                                  :input-pins ',inputs :output-pins ',outputs))))))

           (schem
            (destructuring-bind (schem-name inputs outputs parts-list nets)
                (rest def)
              `(let ((,schem-name (cl-event-passing-user:@new-schematic :name ,schem-name
                                              :input-pins ',inputs :output-pins ',outputs)))
                 ,@(compile-parts schem-name parts-list)
                 ,@(compile-nets schem-name nets))))))
        (compiled-tail (compile-network tail)))
    (if compiled-tail
        (append one (list compiled-tail))
      one)))

(defun compile-inputs (part-name input-list)
  (mapcar #'(lambda (id)
              `(add-input-for-part ,part-name ,id))
          input-list))

(defun compile-outputs (part-name output-list)
  (mapcar #'(lambda (id)
              `(add-output-for-part ,part-name ,id))
          output-list))

(defun compile-parts (schem-name part-list)
  (mapcar #'(lambda (id)
              `(cl-event-passing-user:add-part-to-schematic ,schem-name ,id))
          part-list))

(defun flatten (lis)
  (when lis
    (append (car lis) (flatten (cdr lis)))))

(defun make-receiver (schem-name pair)
  (if (eq 'self (first pair))
      `(make-outbound-receiver ,schem-name ,(second pair))
    `(make-inbound-receiver ,(first pair) ,(second pair))))

(defun make-source (schem-name pair)
  (if (eq 'self (first pair))
      `(make-source-coming-from-outside ,schem-name ,(second pair))
    `(make-source-from-child ,(first pair) ,(second pair))))

(defun compile-nets (schem-name net-list)
  (let ((wires (mapcar #'(lambda (net)
                                    (assert (= 3 (length net)))
                                    ;; if contains SELF, then outgoing receiver, else ingoing
                                    `(,(gensym "WIRE-") (make-wire (list ,@(mapcar #'(lambda (part-pin-pair)
                                                                                       (make-receiver schem-name part-pin-pair))
                                                                                   (third net))))))
                                net-list)))
    (let ((wire-names (mapcar #'first wires))
          (sources-for-each-wire (mapcar #'first net-list)))
      (let ((sources
              (mapcar #'(lambda (wire-name source-list)
                          `(make-sources-for-wire ,wire-name
                                                  ,schem-name
                                                  (list ,@(mapcar #'(lambda (part-pin-pair)
                                                               (make-source schem-name part-pin-pair))
                                                           source-list))))
                      wire-names sources-for-each-wire)))
        `((let (,@wires)
            ,@sources))))))
               


(defun mtest2 ()
  (pprint
   (compile-network '(
                      (schem top (:in :in2) (:out :out2)
                             (child1 child2 child3)
                             (
                              ( ((self :in)) --> ((child :childin)) )

                              ( ((child :childout)) --> ((self :out)) )
                              )
                             )))))


(defun mtest ()
  (pprint
   (compile-network '(
                      (code flow-through-1 (:ftin) (:ftout) #'flow-through)
                      (code flow-through-2 (:ftin) (:ftout) #'flow-through)
                      ))))

(defun mtest3 ()
  (pprint
   (compile-network '(
                      (code flow-through-1 (:ftin) (:ftout) #'flow-through)
                      (code flow-through-2 (:ftin) (:ftout) #'flow-through)

                      (schem child (:childin) (:childout)
                             (flow-through-1 flow-through-2)
                             (
                              ( ((self :childin))        --> ((flow-through-1 :ftin)) )
                              ( ((flow-through-1 :ftout)) --> ((flow-through-2 :ftin)) )
                              ( ((flow-through-2 :ftout)) --> ((self :childout)) )
                              )
                             )

                      (schem top (:in) (:out)
                             (child)
                             (
                              ( ((self :in))  --> ((child :childin)) )
                              ( ((child :childout)) --> ((self :out)) )
                              )
                             )

                      )
                    )))

