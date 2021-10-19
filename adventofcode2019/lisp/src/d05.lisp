(defpackage advent19.d05
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent19.d05)

(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d05.txt"))

(defun part-1 (in)
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                in
                :write-fn (lambda (vmi val)
                            (push val res))))
         (in-ch (advent19.vm:vm-in-ch vmi)))
    (chanl:send in-ch 1)
    (advent19.vm:wait-vm vmi)
    (first res)))

(defun part-2 (in)
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                in
                :write-fn (lambda (vmi val)
                            (push val res))))
         (in-ch (advent19.vm:vm-in-ch vmi)))
    (chanl:send in-ch 5)
    (advent19.vm:wait-vm vmi)
    (first res)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

