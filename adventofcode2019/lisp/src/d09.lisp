(defpackage advent19.d09
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent19.d09)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d09.txt"))

(defun part-1 (in)
  (bind ((res nil)
         (vmi (advent19.vm:start-vm-with
                in
                :write-fn (lambda (vmi val)
                            (push val res)))))
    (advent19.vm:send-vm vmi 1)
    (advent19.vm:wait-vm vmi)
    (reverse res)))

(defun part-2 (in)
  nil)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

