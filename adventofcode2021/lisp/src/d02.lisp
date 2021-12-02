(defpackage advent.d02
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse-move
    :part-1
    :part-2
    :run))
(in-package advent.d02)
(named-readtables:in-readtable :interpol-syntax)

(defun parse-move (s)
  (bind ((s (str:trim s))
         ((dir n) (str:split " " s))
         (n (parse-integer n)))
    (list dir n)))

(defun input ()
  (->>
    (str:from-file "../input/d02.txt")
    (str:split #?|\n|)
    (remove-if #'str:empty?)
    (mapcar #'parse-move)))

(defun part-1 (input)
  (bind ((x 0)
         (depth 0))
    (loop for (dir n) in input do
          (alexandria:eswitch (dir :test #'equal)
            ("up" (decf depth n))
            ("down" (incf depth n))
            ("forward" (incf x n))))
    (* x depth)))

(defun part-2 (input)
  (bind ((x 0)
         (depth 0)
         (aim 0))
    (loop for (dir n) in input do
          (alexandria:eswitch (dir :test #'equal)
            ("down" (incf aim n))
            ("up" (decf aim n))
            ("forward" (progn
                         (incf x n)
                         (incf depth (* aim n))))))
    (* x depth)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

