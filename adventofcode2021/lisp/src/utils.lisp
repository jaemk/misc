(defpackage advent.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :with-timing
    :aget
    :make-str
    :trim-to-nil
    :make-hashset
    :hashset-map
    :hashset-empty?
    :hashset-length
    :hashset-insert
    :hashset-insert-all
    :hashset-get
    :hashset-remove
    ))
(in-package :advent.utils)
(named-readtables:in-readtable :interpol-syntax)


(defun now-millis ()
  (bind ((now (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix now))
     (local-time:timestamp-millisecond now))))


(defun aget (key alist)
  (->> (assoc key alist) rest))


(defun trim-to-nil (s)
  (some->
    s
    str:trim
    ((lambda (s)
       (if (zerop (length s))
         nil
         s)))))


(defmacro get-error-backtrace (e)
  (bind ((s (gensym)))
    `(bind ((,s (make-string-output-stream)))
       (progn
         (trivial-backtrace:print-backtrace ,e :output ,s)
         (get-output-stream-string ,s)))))


(defmacro with-timing (&rest forms)
  (bind ((s (gensym))
         (r (gensym))
         (ms (gensym)))
    `(bind ((,s (advent.utils:now-millis))
            (,r (progn ,@forms))
            (,ms (- (advent.utils:now-millis) ,s)))
       (values ,r ,ms))))

(defun make-str (s &key (capacity nil) (adjustable t))
  (when (and capacity (< capacity (length s)))
    (error #?|capacity: ${capacity} cannot be less than initial str len: ${(length s)}|))
  (bind ((out (make-array (max (length s) (or capacity 0))
                          :adjustable adjustable
                          :fill-pointer (length s)
                          :element-type 'character)))
    (when (or (not capacity) (>= capacity (length s)))
      (loop for c across s
            for i from 0 do
            (setf (aref out i) c)))
    out))


(defclass hashset ()
   ((table
     :initarg :table
     :accessor hashset-table)))


(defun make-hashset (&key (test nil))
  (bind ((test (or test #'equal))
         (table (make-hash-table :test test))
         (set (make-instance 'hashset :table table)))
    set))

(defmethod hashset-map ((hs hashset) f)
  (bind ((table (hashset-table hs)))
    (maphash (lambda (k v) (funcall f k)) table)))

(defmethod hashset-length ((hs hashset))
  (bind ((table (hashset-table hs)))
    (hash-table-count table)))

(defmethod hashset-empty? ((hs hashset))
  (zerop (hashset-length hs)))

(defmethod hashset-insert ((hs hashset) value)
  (bind ((table (hashset-table hs)))
    (setf (gethash value table) t)))

(defmethod hashset-insert-all ((hs hashset) vals)
  (loop for v in vals do
        (hashset-insert hs v)))

(defmethod hashset-get ((hs hashset) value)
  (bind ((table (hashset-table hs)))
    (gethash value table)))

(defmethod hashset-remove ((hs hashset) value)
  (bind ((table (hashset-table hs)))
    (remhash value table)))

