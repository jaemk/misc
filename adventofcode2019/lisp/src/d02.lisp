(in-package advent19.d02)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (->>
    (str:from-file "../input/d02.txt")
    (str:trim)
    (str:split #?",")
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)
    ((lambda (l) (coerce l 'vector)))))

(defun do-add (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (+ in-1-val in-2-val))))

(defun do-mul (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (* in-1-val in-2-val))))

(defun exec (code)
  (bind ((code (copy-seq code))
         (ptr 0)
         (done nil))
    (loop
      do
        (progn
          (bind ((v (aref code ptr)))
            (case v
              (99 (setf done t))
              (1 (progn
                   (do-add ptr code)
                   (setf ptr (+ 4 ptr))))
              (2 (progn
                   (do-mul ptr code)
                   (setf ptr (+ 4 ptr)))))))
      when done
        return code)))


(defun prepare (code noun verb)
  (setf (aref code 1) noun)
  (setf (aref code 2) verb)
  code)

(defun exec-with (code noun verb)
  (->
    code
    (prepare noun verb)
    (exec)
    (aref 0)))

(defun part-1 (in)
  (->
    in
    (exec-with 12 2)))

(defun part-2 (in)
  (bind ((result nil))
    (loop
      for noun from 0 to 99
      do
        (->
          (loop
            for verb from 0 to 99
            when (= 19690720 (exec-with in noun verb))
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

