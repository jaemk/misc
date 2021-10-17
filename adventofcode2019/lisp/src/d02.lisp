(defpackage advent19.d02
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :exec
    :part-1
    :part-2
    :run))
(in-package advent19.d02)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d02.txt"))

(defun exec (code &optional noun verb)
  (->
    code
    (advent19.vm:run-vm-with :noun noun :verb verb)
    (advent19.vm:vm-code)
    (aref 0)))

(defun part-1 (in)
  (->
    in
    (exec 12 2)))

(defun part-2 (in)
  (bind ((result nil))
    (loop
      for noun from 0 to 99
      do
        (->
          (loop
            for verb from 0 to 99
            when (= 19690720 (exec in noun verb))
              return (+ (* 100 noun) verb))
          ((lambda (res) (when res (setf result res)))))
      when result
        return result)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

