(defpackage advent.d10
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run)
  (:import-from :advent.utils :make-hashset :hashset-get))
(in-package advent.d10)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar #'str:trim)
    (remove-if #'str:empty?)))

(defun input ()
  (->>
    (str:from-file "../input/d10.txt")
    (parse)))

(defparameter *closing* (make-hashset :from (list #.(code-char 41)  ;; )
                                                  #.(code-char 93)  ;; ]
                                                  #.(code-char 125) ;; }
                                                  #.(code-char 62)  ;; >
                                                  )))

(defparameter *scores* (list (cons #.(code-char 41) 3)
                             (cons #.(code-char 93) 57)
                             (cons #.(code-char 125) 1197)
                             (cons #.(code-char 62) 25137)))

(defun valid-pair? (a b)
  ;; valid pairs are at most 2 away from one another
  (>= 2 (abs (- (char-int a) (char-int b)))))

(defun part-1 (input)
  (bind ((invalid nil)
         (valid nil))
    (loop for line in input do
          (block check-line
            (bind ((stack nil))
              (loop for c across line do
                    (if (not (hashset-get *closing* c))
                      (push c stack)
                      (bind ((prev (pop stack)))
                        (when (not (valid-pair? c prev))
                          (push c invalid)
                          (return-from check-line))))))
            (push line valid)))
    (values
      (apply #'+ (mapcar (lambda (c) (advent.utils:aget c *scores*)) invalid))
      valid)))

(defun pair (c)
  (ecase c
    (#.(code-char 40) #.(code-char 41))
    (#.(code-char 91) #.(code-char 93))
    (#.(code-char 123) #.(code-char 125))
    (#.(code-char 60) #.(code-char 62))))

(defun complete (line)
  (bind ((stack nil))
    (loop for c across line do
          (if (hashset-get *closing* c)
            (bind ((prev (pop stack)))
              (when (not (valid-pair? prev c))
                (error (format nil "invalid pair: ~a, ~a" prev c))))
            (push c stack)))
    (loop for c in stack collect (pair c))))

(defun score (tail)
  (bind ((n 0))
    (loop for c in tail do
          (progn
            (setf n (* n 5))
            (incf n
                  (ecase c
                    (#.(code-char 41) 1)
                    (#.(code-char 93) 2)
                    (#.(code-char 125) 3)
                    (#.(code-char 62) 4)))))
    n))

(defun part-2 (input)
  (bind (((:values _ valid) (part-1 input))
         (tails (mapcar #'complete valid))
         (scores (mapcar #'score tails))
         (size (length scores))
         (middle (-> (/ size 2) #'floor)))
    (nth middle (sort scores #'<))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

