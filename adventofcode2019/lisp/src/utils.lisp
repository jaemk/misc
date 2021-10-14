(in-package :advent19.utils)

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
    `(bind ((,s (advent19.utils:now-millis))
            (,r (progn ,@forms))
            (,ms (- (advent19.utils:now-millis) ,s)))
       (values ,r ,ms))))

