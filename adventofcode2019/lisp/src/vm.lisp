(defpackage advent19.vm
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :make-vm
    :read-code-from-file
    :run-vm
    :run-vm-with
    :vm-code))
(in-package advent19.vm)
(named-readtables:in-readtable :interpol-syntax)

(defclass vm ()
  ((code
     :initarg :code
     :initform (vector)
     :accessor vm-code)))

(defun read-code-from-file (file-path)
  (->>
    (str:from-file file-path)
    (str:trim)
    (str:split #?",")
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)
    ((lambda (l) (coerce l 'vector)))))

(defun make-vm (code &key (noun nil) (verb nil))
  (bind ((vmi (make-instance 'vm :code (coerce (copy-seq code) 'vector))))
    (when noun
      (setf (aref (vm-code vmi) 1) noun))
    (when verb
      (setf (aref (vm-code vmi) 2) verb))
    vmi))

(defun run-vm-with (code &key (noun nil) (verb nil))
    (-> (make-vm code :noun noun :verb verb)
        (run-vm)))

(defmethod run-vm ((o vm))
  (bind ((code (vm-code o))
         (ptr 0))
    (loop
      do
        (progn
          (bind ((v (aref code ptr)))
            (case v
              (99 (return))
              (1 (progn
                   (do-add ptr code)
                   (setf ptr (+ 4 ptr))))
              (2 (progn
                   (do-mul ptr code)
                   (setf ptr (+ 4 ptr))))))))
    o))


(defun do-add (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (+ in-1-val in-2-val))))

(defun do-mul (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (* in-1-val in-2-val))))

