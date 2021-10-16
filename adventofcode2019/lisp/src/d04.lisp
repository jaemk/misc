(defpackage advent19.d04
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent19.d04)

(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->> s
    (str:trim)
    (str:split "-")
    (mapcar #'parse-integer)))

(defun input ()
  (->>
    (str:from-file "../input/d04.txt")
    (parse)))

(defun valid-a? (pw-int)
  (bind ((pw (map 'list #'char-int #?"${pw-int}"))
         (prev #.(char-int #\0))
         (has-double nil))
    (when (= 6 (length pw))
      (if (loop
            for c in pw
            do (progn
                 (bind ((p prev))
                   (setf prev c)
                   (when (= c p)
                     (setf has-double t))
                   (when (< c p)
                     (return t)))))
          nil
          has-double))))

(defun part-1 (in)
  (bind (((start end) in)
         (seen (make-hash-table :size (- end start)))
         (num 0))
    (loop for pw from (+ 1 start) below end do
          (when (not (gethash pw seen))
            (setf (gethash pw seen) t)
            (when (valid-a? pw)
              (incf num))))
    num))

(defun part-2 (in)
  in)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

