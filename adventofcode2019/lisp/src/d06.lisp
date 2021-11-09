(defpackage advent19.d06
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :parents
    :nearest-common-ancestor
    :part-1
    :part-2
    :run))
(in-package advent19.d06)
(named-readtables:in-readtable :interpol-syntax)

(defclass planet ()
  ((name
     :initarg :name
     :initform (error "planet name is required")
     :accessor planet-name)
   (parent
     :initarg :parent
     :initform nil
     :accessor planet-parent)
   (children
     :initarg :children
     :initform nil
     :accessor planet-children)))

(defun make-planet (name &key (parent nil) (children nil))
  (bind ((p (make-instance 'planet :name name)))
    (when parent
      (setf (planet-parent p) parent))
    (when children
      (setf (planet-children p) children))
    p))

(defmethod add-child ((p planet) child)
  (push (planet-children p) child))


(defun get-or-create-planet (planets-map name &key (parent nil) (child nil))
  (alexandria:if-let (p (gethash name planets-map))
    (progn
      (when child
        (push child (planet-children p)))
      (when parent
        (setf (planet-parent p) parent))
      p)
    (bind ((children (if child (list child) nil))
           (p (make-planet name :parent parent :children children)))
      (setf (gethash name planets-map) p)
      p)))

(defun parse (s)
  (bind ((lines (->> (str:trim s) (str:split #?|\n|) (remove-if #'str:empty?)))
         (planets (make-hash-table :test #'equal)))
    (loop for relation in lines
          do (bind (((parent child) (str:split ")" relation)))
               (get-or-create-planet planets parent :child child)
               (get-or-create-planet planets child :parent parent)))
    planets))

(defun input ()
  (->>
    (str:from-file "../input/d06.txt")
    (parse)))

(defun count-depth (planets root depth)
  (bind ((p (gethash root planets))
         (children (planet-children p)))
    (->>
      (mapcar
        (lambda (p) (count-depth planets p (+ 1 depth)))
        children)
      (reduce #'+)
      (+ depth))))

(defun part-1 (in)
  (count-depth in "COM" 0))

(defun parents (planets p)
  (bind ((parents nil)
         (pname p))
    (loop
      do (bind ((p (gethash pname planets))
                (parent (planet-parent p)))
           (if (not parent)
             (return)
             (progn
               (push parent parents)
               (setf pname parent)))))
    parents))

(defun nearest-common-ancestor (planets a b)
  (bind ((a-parents (parents planets a))
         (b-parents (parents planets b))
         (nca (first a-parents)))
    (loop for a in a-parents
          for b in b-parents
          do (if (equal a b)
               (setf nca a)
               (return)))
    nca))

(defun depth (planets p &key from)
  (bind ((d 0)
         (pname p))
    (loop
      do (bind ((p (gethash pname planets))
                (parent (planet-parent p)))
           (if (equal from parent)
             (return)
             (progn
               (incf d)
               (setf pname parent)))))
    d))

(defun part-2 (in)
  (bind ((nca (nearest-common-ancestor in "YOU" "SAN")))
    (+
      (depth in "YOU" :from nca)
      (depth in "SAN" :from nca))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

