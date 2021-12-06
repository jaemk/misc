(defpackage advent
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main))
(in-package :advent)
(named-readtables:in-readtable :interpol-syntax)


(defparameter *days*
  '(
    advent.d01:run
    advent.d02:run
    advent.d03:run
    advent.d04:run
    ))


(defun run ()
  (loop for (day run-day) in (mapcar #'list (alexandria:iota (length *days*) :start 1) *days*) do
    (format t "~&=======================~%* Day ~a *~%" day)
    (bind (((:values r ms) (advent.utils:with-timing (funcall run-day))))
      (format t "~&>>> ~ams~%" ms))
    ))


(defun main (argvs)
  ;; handle any errors if they aren't cause by the catch-all handler in 'main
  (setf
    *debugger-hook*
    (lambda (c old-hook)
      (declare (ignore old-hook))
      (format *error-output* "~&Unhandled error: ~a~%" (advent.utils:get-error-backtrace c))
      (sb-ext:quit :unix-status 1)))

  (handler-bind
    (
      ;; C-c
      (sb-sys:interactive-interrupt
        (lambda (c)
          (format t "~&Aborting...~%")
          (sb-ext:quit :unix-status 1)))

      ;; everything else
      (error
        (lambda (c)
          (format *error-output* "~&Error: ~a~%" (advent.utils:get-error-backtrace c))
          (sb-ext:quit :unix-status 1)))
    )
      (progn
        (log:config (advent.config:value :log-level))
        (log:config :sane2)
        (log:config :nofile)
        (log:debug "args: ~a" argvs)
        (run))))

