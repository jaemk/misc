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
  (bind ((found nil))
    (loop for line in input do
          (bind ((stack nil))
            (loop for c across line do
                  (if (not (hashset-get *closing* c))
                    (push c stack)
                    (bind ((prev (pop stack)))
                      (when (not (valid-pair? c prev))
                        (push c found)
                        (return)))))))
    (apply #'+ (mapcar (lambda (c) (advent.utils:aget c *scores*)) found))))

(defun part-2 (input)
  nil)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

