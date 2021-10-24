(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d05 :in all)

(test test-day05-parse-op-01
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 99)))
    (is (eql 99 op))
    (is (eql :pos m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-02
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1002)))
    (is (eql 2 op))
    (is (eql :pos m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-03
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1132)))
    (is (eql 32 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-04
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 11103)))
    (is (eql 3 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :imd m3))))

(test test-day05-parse-op-05
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 101)))
    (is (eql 1 op))
    (is (eql :imd m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day05-modes-01
  (is (equalp '(1002 4 3 4 99)
              (->
                (advent19.vm:run-vm-with #(1002 4 3 4 33))
                (advent19.vm:vm-mem-range 0 5)))))

(test test-day05-modes-02
  (is (equalp '(1101 100 -1 4 99)
              (->
                (advent19.vm:run-vm-with #(1101 100 -1 4 0))
                (advent19.vm:vm-mem-range 0 5)))))

(test test-day05-part1-01
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                #(3 0 4 0 99)
                :write-fn (lambda (vmi val)
                            (declare (ignore vmi))
                            (push val res))))
         (in-ch (advent19.vm:get-vm-in-ch vmi))
         (input-value 123))
    (chanl:send in-ch input-value)
    (advent19.vm:wait-vm vmi)
    (is (eql 1 (length res)))
    (is (eql input-value (first res)))))

(test test-day05-part1-real
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                (advent19.d05:input)
                :write-fn (lambda (vmi val)
                            (when (and (< 0 val) (not (= 15386262 val)))
                              (format (advent19.vm:get-vm-stdout vmi) "~&****ERROR***** ~a~&" val))
                            (push val res))))
         (in-ch (advent19.vm:get-vm-in-ch vmi)))
    (chanl:send in-ch 1)
    (advent19.vm:wait-vm vmi)
    (is (eql 15386262 (reduce #'+ res)))
    (is (eql 15386262 (first res)))))

(test test-day05-part2-01
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                #(
                  3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99
                  )
                :write-fn (lambda (vmi val)
                            (declare (ignore vmi))
                            (push val res))))
         (in-ch (advent19.vm:get-vm-in-ch vmi)))
    ;; less-than 8 => 999
    (chanl:send in-ch 7)
    (advent19.vm:wait-vm vmi)
    (is (eql 999 (first res)))

    ;; equal 8 => 1000
    (advent19.vm:reset-vm vmi)
    (advent19.vm:start-vm vmi)
    (chanl:send in-ch 8)
    (advent19.vm:wait-vm vmi)
    (is (eql 1000 (first res)))
    (advent19.vm:reset-vm vmi)

    ;; greater-than 8 => 1001
    (advent19.vm:reset-vm vmi)
    (advent19.vm:start-vm vmi)
    (chanl:send in-ch 9)
    (advent19.vm:wait-vm vmi)
    (is (eql 1001 (first res)))
    (advent19.vm:reset-vm vmi)))

(test test-day05-part2-real
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                (advent19.d05:input)
                :write-fn (lambda (vmi val)
                            (when (and (< 0 val) (not (= 10376124 val)))
                              (format (advent19.vm:get-vm-stdout vmi) "~&****ERROR***** ~a~&" val))
                            (push val res))))
         (in-ch (advent19.vm:get-vm-in-ch vmi)))
    (chanl:send in-ch 5)
    (advent19.vm:wait-vm vmi)
    (is (eql 10376124 (reduce #'+ res)))
    (is (eql 10376124 (first res)))))

