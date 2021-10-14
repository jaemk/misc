(in-package advent19.d03)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (->>
    (str:from-file "../input/d03.txt")
    (str:trim)))

(defun part-1 (in)
  (->
    in))

(defun part-2 (in)
  (->
    in))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

