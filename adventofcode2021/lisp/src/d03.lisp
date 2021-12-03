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
  (->>
    (str:from-file "../input/d03.txt")
    (str:split #?|\n|)
    (remove-if #'str:empty?)
    (map 'vector
         (lambda (line)
           (map 'vector
                (lambda (c)
                  (- (char-int c)
                     #.(char-int #\0)))
                line)))))

(defun part-1 (input)
  (bind ((gs (advent.utils:make-adjustable-string ""))
         (es (advent.utils:make-adjustable-string "")))
    (loop for i from 0 to (1- (length (aref input 0))) do
          (bind ((ones 0)
                 (zeros 0))
            (loop for line across input
                  if (= 1 (aref line i))
                    do (incf ones)
                  else
                    do (incf zeros))
            (if (> ones zeros)
              (progn
                (vector-push-extend #\1 gs)
                (vector-push-extend #\0 es))
              (progn
                (vector-push-extend #\0 gs)
                (vector-push-extend #\1 es)))))
    (* (parse-integer gs :radix 2)
       (parse-integer es :radix 2))))

(defun part-2 (input)
  (bind ((bit-length (length (aref input 0))))
    (flet ((bit-rating (kind)
             (bind ((input (copy-seq input)))
               (loop for i from 0 to (1- bit-length)
                     when (= 1 (length input))
                       do (return)
                     do
                       (bind ((ones 0)
                              (zeros 0))
                         (loop for line across input
                               if (= 1 (aref line i))
                                 do (incf ones)
                               else
                                 do (incf zeros))
                         (cond
                           ((equal :o2 kind)
                              (cond
                                ((>= ones zeros)
                                  ; keep ones
                                  (setf input
                                        (remove-if
                                          (lambda (row)
                                            (not (= 1 (aref row i))))
                                          input)))
                                ((> zeros ones)
                                  ; keep zeros
                                  (setf input
                                        (remove-if
                                          (lambda (row)
                                            (not (= 0 (aref row i))))
                                          input)))
                                ;; keep everything
                                (t nil)))
                           ((equal :co2 kind)
                              (cond
                                ((< ones zeros)
                                  ; keep ones
                                  (setf input
                                        (remove-if
                                          (lambda (row)
                                            (not (= 1 (aref row i))))
                                          input)))
                                ((<= zeros ones)
                                  ; keep zeros
                                  (setf input
                                        (remove-if
                                          (lambda (row)
                                            (not (= 0 (aref row i))))
                                          input)))
                                ;; keep everything
                                (t nil)))
                           (t (error #?|invalid kind ${kind}|)))))
               (aref input 0))))
      (bind ((o2 (coerce (bit-rating :o2) 'list))
             (co2 (coerce (bit-rating :co2) 'list)))
        (* (parse-integer (format nil "~{~a~}" o2) :radix 2)
           (parse-integer (format nil "~{~a~}" co2) :radix 2))))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

