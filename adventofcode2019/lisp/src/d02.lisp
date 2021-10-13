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
         (in-2-val (aref code in-2-ptr))
         )
    (setf (aref code in-3-ptr) (+ in-1-val in-2-val))))

(defun do-mul (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr))
         )
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
                   (setf ptr (+ 4 ptr))))
              )))
      when done
        return code
      )))


(defun part-1 (in)
  (->>
    in
    (mapcar #'mass->fuel)
    (reduce #'+)))

(defun part-2 (in)
  (->>
    in
    ))

(defun run ()
  (let ((in (input)))
    (format t "~&Part 1: ~a" (part-1 in))
    (format t "~&Part 2: ~a" (part-2 in))))

