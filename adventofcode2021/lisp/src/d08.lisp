(defpackage advent.d08
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run)
  (:import-from :advent.utils
                :make-hashset
                :hashset-equal
                :hashset-insert
                :hashset-insert-all
                :hashset-get
                :hashset-remove
                :hashset-length
                :hashset-empty?
                :hashset-difference
                :hashset->list))
(in-package advent.d08)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->>
    (str:split #?|\n| s)
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (mapcar (lambda (line)
              (bind (((left right) (str:split "|" line))
                     (left (->> (str:trim left) (str:split " ")))
                     (right (->> (str:trim right) (str:split " "))))
                (list left right))))))

(defun input ()
  (->>
    (str:from-file "../input/d08.txt")
    (parse)))

(defun part-1 (input)
  (loop for line in input sum
        (loop for digit in (second line) sum
              (case (length digit)
                (2 1) ; 1
                (4 1) ; 4
                (3 1) ; 7
                (7 1) ; 8
                (otherwise 0)))))


; 2 -> 1
; 3 -> 7
; 4 -> 4
; 5 -> 2, 3, 5
; 6 -> 0, 6, 9
; 7 -> 8
;   0:      1:      2:      3:      4:
;  aaaa    ....    aaaa    aaaa    ....
; b    c  .    c  .    c  .    c  b    c
; b    c  .    c  .    c  .    c  b    c
;  ....    ....    dddd    dddd    dddd
; e    f  .    f  e    .  .    f  .    f
; e    f  .    f  e    .  .    f  .    f
;  gggg    ....    gggg    gggg    ....
;
;   5:      6:      7:      8:      9:
;  aaaa    aaaa    aaaa    aaaa    aaaa
; b    .  b    .  .    c  b    c  b    c
; b    .  b    .  .    c  b    c  b    c
;  dddd    dddd    ....    dddd    dddd
; .    f  e    f  .    f  e    f  .    f
; .    f  e    f  .    f  e    f  .    f
;  gggg    gggg    ....    gggg    gggg
;
(defparameter *zero*
  (make-hashset :from (list #\a #\b #\c #\e #\f #\g)))

(defparameter *one*
  (make-hashset :from (list #\c #\f)))

(defparameter *two*
  (make-hashset :from (list #\a #\c #\d #\e #\g)))

(defparameter *three*
  (make-hashset :from (list #\a #\c #\d #\f #\g)))

(defparameter *four*
  (make-hashset :from (list #\b #\d #\c #\f)))

(defparameter *five*
  (make-hashset :from (list #\a #\b #\d #\f #\g)))

(defparameter *six*
  (make-hashset :from (list #\a #\b #\d #\e #\f #\g)))

(defparameter *seven*
  (make-hashset :from (list #\a #\c #\f)))

(defparameter *eight*
  (make-hashset :from (list #\a #\b #\c #\d #\e #\f #\g)))

(defparameter *nine*
  (make-hashset :from (list #\a #\b #\c #\d #\f #\g)))

(defun by-length (digits)
  (bind ((m (make-hash-table)))
    (loop for d in digits do
          (push d (gethash (length d) m)))
    m))

(defun map->list (map)
  (bind ((res nil))
    (maphash (lambda (k v) (push (list k v) res)) map)
    res))

(defun decode (line)
  (bind (((left right) line)
         (all (append left right))
         (by-len (by-length all))
         (temp1 (make-hashset))
         (temp2 (make-hashset))
         (temp3 (make-hashset))
         (temp4 (make-hashset))
         (found (make-hashset))
         (key (make-hash-table :test #'equal))
         (sevens (gethash 7 by-len))
         (sixes (gethash 6 by-len))
         (fives (gethash 5 by-len))
         (fours (gethash 4 by-len))
         (threes (gethash 3 by-len))
         (twos (gethash 2 by-len))
         )
    ;; collect segments of 8
    (loop for c across (first sevens) do (hashset-insert temp1 c))
    ;; remove 0, 6, and 9 from those segments to find the possible
    ;; values for e-d-c
    (loop for digit in sixes do
          (bind ((chars (loop for c across digit collect c))
                 (hs (make-hashset)))
            (hashset-insert-all hs chars)
            (->>
              (hashset-difference temp1 hs)
              (hashset->list)
              (first)
              (hashset-insert temp2))))
    ;; now find which segment of 1 is present, this is the value of c.
    ;; the value that isn't present is f
    (loop for c across (first twos) do
          (if (hashset-get temp2 c)
            (progn
              (setf (gethash c key) #\c)
              (hashset-insert found c))
            (progn
              (setf (gethash c key) #\f)
              (hashset-insert found c))))
    (when (> 2 (hashset-length found))
      (error "expected to determine c and f.."))
    ;; now find which segment of 7 is missing, this is the value of a
    (loop for c across (first threes) do
          (when (not (hashset-get found c))
            (hashset-insert found c)
            (setf (gethash c key) #\a)))
    (when (> 3 (hashset-length found))
      (error "expected to determine a, c, and f.."))
    ;; now find the 5-segments: 2, 3, 5.
    ;; we know a, c, and f.
    ;; for the intersection of 2, 3, 5,
    ;; the two segments we don't know are d and g
    (loop for digit in fives do
          (bind ((new (make-hashset)))
            (if (hashset-empty? temp3)
              (loop for c across digit do (hashset-insert temp3 c))
              (progn
                (loop for c across digit do
                      (when (and (hashset-get temp3 c) (not (hashset-get found c)))
                        (hashset-insert new c)))
                (setf temp3 new)))))
    ;; now find 4 and remove any known segments
    ;; then the segment that overlaps with temp3 is d and what doesn't is b
    (loop for c across (first fours) do
          (when (not (hashset-get found c))
            (hashset-insert temp4 c)))
    (loop for c in (hashset->list temp4) do
          (if (hashset-get temp3 c)
            (progn
              (setf (gethash c key) #\d)
              (hashset-insert found c))
            (progn
              (setf (gethash c key) #\b)
              (hashset-insert found c))))
    ;; the other segment in temp3 that we haven't found yet is g
    (loop for c in (hashset->list temp3) do
          (when (not (hashset-get found c))
            (setf (gethash c key) #\g)
            (hashset-insert found c)))
    ;; the last segment in 8 that hasn't been found is e
    (loop for c across (first sevens) do
          (when (not (hashset-get found c))
            (setf (gethash c key) #\e)
            (hashset-insert found c)))
    (loop for digit in right collect
          (loop for c across digit collect
                (gethash c key)))))

(defun interpret (digits)
  (str:join ""
    (loop for d in digits collect
          (bind ((hs (make-hashset :from d)))
            (cond
              ((hashset-equal *zero* hs) "0")
              ((hashset-equal *one* hs) "1")
              ((hashset-equal *two* hs) "2")
              ((hashset-equal *three* hs) "3")
              ((hashset-equal *four* hs) "4")
              ((hashset-equal *five* hs) "5")
              ((hashset-equal *six* hs) "6")
              ((hashset-equal *seven* hs) "7")
              ((hashset-equal *eight* hs) "8")
              ((hashset-equal *nine* hs) "9")
              )))))

(defun part-2 (input)
  (loop for line in input sum
        (->>
          (decode line)
          (interpret)
          (parse-integer))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

