(defpackage advent.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :with-timing
    :aget
    :make-str
    :trim-to-nil))
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

