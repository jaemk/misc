(in-package advent19.d01)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (->>
    (str:from-file "../input/d01.txt")
    (str:split #?"\n")
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)))

(defun mass->fuel (m)
  (if (<= m 6)
    0
    (-> (/ m 3)
        floor
        (- 2))))

(defun mass->total-fuel (m f)
  (cond
    ((null f)
     (bind ((new-f (mass->fuel m)))
        (mass->total-fuel new-f new-f)))
    ((zerop f) m)
    (t
     (bind ((new-f (mass->fuel f)))
        (mass->total-fuel (+ m new-f) new-f)))))

(defun part-1 (in)
  (->>
    in
    (mapcar #'mass->fuel)
    (reduce #'+)))

(defun part-2 (in)
  (->>
    in
    (mapcar (lambda (m) (mass->total-fuel m nil)))
    (reduce #'+)))

(defun run ()
  (let ((in (input)))
    (format t "~&Part 1: ~a" (part-1 in))
    (format t "~&Part 2: ~a" (part-2 in))))

