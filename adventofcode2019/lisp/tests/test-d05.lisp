(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d05 :in all)

(test test-day05-part1-01
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with #(3 0 4 0 99) :write-fn (lambda (vmi val) (push val res))))
         (in-ch (advent19.vm:vm-in-ch vmi))
         (input-value 123))
    (chanl:send in-ch input-value)
    (advent19.vm:wait-vm vmi)
    (is (equalp (list input-value) res))))

