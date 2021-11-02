(defpackage advent19.d12
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :step-bodies
    :format-bodies
    :part-1
    :part-2
    :run))
(in-package advent19.d12)
(named-readtables:in-readtable :interpol-syntax)

(defclass body ()
  (
   (x
     :initarg :x
     :initform (error "body :x required")
     :accessor body-x)
   (y
     :initarg :y
     :initform (error "body :y required")
     :accessor body-y)
   (z
     :initarg :z
     :initform (error "body :z required")
     :accessor body-z)
   (dx
     :initarg :dx
     :initform 0
     :accessor body-dx)
   (dy
     :initarg :dy
     :initform 0
     :accessor body-dy)
   (dz
     :initarg :dz
     :initform 0
     :accessor body-dz)
   ))

(defmethod print-object ((b body) stream)
  (print-unreadable-object (b stream)
    (with-accessors ((x body-x)
                     (y body-y)
                     (z body-z)
                     (dx body-dx)
                     (dy body-dy)
                     (dz body-dz)) b
      (format stream
              "pos=<x=~a, y=~a, z=~a>, vel=<x=~a, y=~a, z=~a>"
              x y z dx dy dz))))

(defun format-bodies (bodies)
  (bind ((s (make-string-output-stream)))
    (loop for b in bodies
          do (format s "~&~a" b))
    (get-output-stream-string s)))


;; <x=14, y=4, z=5>
(defun parse-body (s)
  (flet ((parse-num (num)
           (-> (str:trim num) (str:s-rest) (str:s-rest) (parse-integer))))
    (bind ((s (str:trim s))
           (s-len (length s))
           (s (str:substring 1 (1- s-len) s))
           ((rawx rawy rawz) (str:split "," s))
           (x (parse-num rawx))
           (y (parse-num rawy))
           (z (parse-num rawz))
           )
      (make-instance 'body :x x :y y :z z)
      )))

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?"\n")
    (mapcar #'parse-body)))

(defun input ()
  (->
    (str:from-file "../input/d12.txt")
    (parse)))

(defun step-bodies (bodies)
  (flet ((compare-update-vels (this other)
           (cond
             ((> (body-x this) (body-x other))
                (progn (incf (body-dx other)) (decf (body-dx this))))
             ((< (body-x this) (body-x other))
                (progn (incf (body-dx this)) (decf (body-dx other)))))
           (cond
             ((> (body-y this) (body-y other))
                (progn (incf (body-dy other)) (decf (body-dy this))))
             ((< (body-y this) (body-y other))
                (progn (incf (body-dy this)) (decf (body-dy other)))))
           (cond
             ((> (body-z this) (body-z other))
                (progn (incf (body-dz other)) (decf (body-dz this))))
             ((< (body-z this) (body-z other))
                (progn (incf (body-dz this)) (decf (body-dz other)))))
           ))
    (mapl
      (lambda (bods)
        (bind ((b (first bods)))
          (loop for bb in (rest bods)
                do (compare-update-vels b bb))))
      bodies)
    (mapc
      (lambda (bod)
        (incf (body-x bod) (body-dx bod))
        (incf (body-y bod) (body-dy bod))
        (incf (body-z bod) (body-dz bod)))
      bodies)
    bodies))

(defun calculate-pe (b)
  (+
    (abs (body-x b))
    (abs (body-y b))
    (abs (body-z b))))

(defun calculate-ke (b)
  (+
    (abs (body-dx b))
    (abs (body-dy b))
    (abs (body-dz b))))

(defun calculate-total-energy (b)
  (* (calculate-ke b)
     (calculate-pe b)))

(defun part-1 (in &optional (steps 1000))
  (loop for i from 1 to steps
        do (step-bodies in))
  (apply #'+ (mapcar #'calculate-total-energy in)))

(defun part-2 (in)
  nil)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

