(in-package advent19.d03)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->> s
    (str:trim)
    (str:split #?"\n")
    (mapcar #'str:trim)
    (mapcar (lambda (s) (str:split #?"," s)))))

(defun input ()
  (->>
    (str:from-file "../input/d03.txt")
    (parse)))

(defun find-intersections (moves seen can-intersect)
  "return intersections"
  (bind ((loc (list 0 0))
         (steps 0)
         (ints nil))
    (loop for m in moves do
      (progn
        (bind ((dir (str:s-first m))
               (amount (parse-integer (str:s-rest m))))
          (loop for _ from 1 to amount do
            (progn
              (incf steps)
              (cond
                ((string= dir "U") (incf (second loc)))
                ((string= dir "D") (decf (second loc)))
                ((string= dir "L") (decf (first loc)))
                ((string= dir "R") (incf (first loc))))
              (let ((loc- (copy-list loc)))
                (if can-intersect
                  (let ((prev-steps (gethash loc- seen)))
                    (when prev-steps
                      (push (list (+ steps prev-steps) loc-) ints)))
                  (setf (gethash loc- seen) steps))))))))
    ints))

(defun from-zero (point)
  (bind (((steps point) point))
    (list (apply #'+ (mapcar #'abs point)) steps point)))

(defun part-1 (in)
  (bind (((a b) in)
         (seen (make-hash-table :test #'equal))
         (intsa (find-intersections a seen nil))
         (intsb (find-intersections b seen t))
         (ints (union intsa intsb))
         (dist-steps-ints (mapcar #'from-zero ints)))
    (first (first (sort dist-steps-ints #'< :key #'first)))))

(defun part-2 (in)
  (bind (((a b) in)
         (seen (make-hash-table :test #'equal))
         (intsa (find-intersections a seen nil))
         (intsb (find-intersections b seen t))
         (ints (union intsa intsb))
         (dist-steps-ints (mapcar #'from-zero ints)))
    (second (first (sort dist-steps-ints #'< :key #'second)))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

