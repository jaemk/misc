(defpackage advent19.d08
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))
(in-package advent19.d08)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (in &key (size '(25 6)))
  (bind ((in (map 'list #'digit-char-p in))
         (total-size (length in))
         (layer-size (apply #'* size)))
    (loop for start from 0 to total-size by layer-size
          for end = (+ start layer-size)
          when (>= total-size end)
            collect (subseq in start end))))

(defun input ()
  (->>
    (str:from-file "../input/d08.txt")
    (str:trim)
    (parse)))

(defun part-1 (layers)
  (bind ((fewest-zero-count nil)
         (ones-twos nil))
    (loop for layer in layers
          do (bind ((zero-count 0)
                    (ones-count 0)
                    (twos-count 0))
               (loop for c in layer
                     when (= c 0)
                       do (incf zero-count)
                     when (= c 1)
                       do (incf ones-count)
                     when (= c 2)
                       do (incf twos-count))
               (when (or (null fewest-zero-count) (> fewest-zero-count zero-count))
                 (setf fewest-zero-count zero-count)
                 (setf ones-twos (* ones-count twos-count)))))
    ones-twos))

(defun format-layer (layer &key (size '(25 6)))
  (bind ((layer (mapcar (lambda (n) (if (zerop n) " " "X")) layer))
         (len (length layer))
         ((w h) size))
    (->>
      (loop for start from 0 to len by w
            for end = (+ start w)
            when (>= len end)
              collect (format nil "~{~a~}" (subseq layer start end)))
      (str:join #?"\n"))))

(defun part-2 (layers &key (display '(25 6)))
  (bind ((final (first layers))
         (layers (rest layers))
         (done nil))
    (loop for layer in layers
          when (not done)
            do (bind ((remaining 0))
                 (mapl (lambda (fin lay)
                         (when (= 2 (first fin))
                           (if (not (= 2 (first lay)))
                             (setf (first fin) (first lay))
                             (incf remaining))))
                       final
                       layer)
                 (when (zerop remaining)
                   (setf done t))))
    (if display
      (->>
        (format-layer final :size display)
        (format nil "~%~a"))
      final)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

