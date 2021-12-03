(defpackage advent.d03
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent.d03)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (bind ((bit-length 0)
         (data (->>
                 (str:from-file "../input/d03.txt")
                 (str:split #?|\n|)
                 (remove-if #'str:empty?)
                 (mapcar
                   (lambda (line)
                     (setf bit-length (max bit-length (length line)))
                     (parse-integer line :radix 2))))))
    (list bit-length data)))

(defun majority-ones? (numbers mask)
  (bind ((ones (loop for n in numbers sum (if (< 0 (logand n mask)) 1 0)))
         (half (/ (length numbers) 2)))
    (values (> ones half)
            (= ones half))))

(defun part-1 (input)
  (bind (((bit-length data) input)
         (gs (advent.utils:make-str "" :capacity bit-length))
         (es (advent.utils:make-str "" :capacity bit-length)))
    (loop for i from 0 to (1- bit-length) do
          (if (majority-ones? data (ash 1 i))
            (progn
              (vector-push-extend #\1 gs)
              (vector-push-extend #\0 es))
            (progn
              (vector-push-extend #\0 gs)
              (vector-push-extend #\1 es))))
    (* (parse-integer (reverse gs) :radix 2)
       (parse-integer (reverse es) :radix 2))))

(defun part-2 (input)
  (bind (((bit-length data) input))
    (flet ((bit-rating (kind)
             (bind ((data (copy-list data)))
               (loop for i from 1 to bit-length
                     while (> (length data) 1) do
                     ;; mask moving from the left of a LE representation
                     (bind ((mask (ash 1 (- bit-length i)))
                            ((:values more-ones equal-ones) (majority-ones? data mask)))
                       (cond
                         ((equal :o2 kind)
                            (if (or more-ones equal-ones)
                              ;; keep ones
                              (setf data (remove-if (lambda (n) (zerop (logand n mask))) data))
                              ;; keep zeros
                              (setf data (remove-if (lambda (n) (-> (logand n mask) (logxor mask) (zerop))) data))))
                         ((equal :co2 kind)
                            (if (or more-ones equal-ones)
                              ;; keep zeros
                              (setf data (remove-if (lambda (n) (-> (logand n mask) (logxor mask) (zerop))) data))
                              ;; keep ones
                              (setf data (remove-if (lambda (n) (zerop (logand n mask))) data))))
                         (t (error #?|invalid kind ${kind}|)))))
               (first data))))
      (bind ((o2 (bit-rating :o2))
             (co2 (bit-rating :co2)))
        (* o2 co2)))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

