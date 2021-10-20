(load "deps.lisp")

(use-package 'arrow-macros)
(use-package 'metabang-bind)
(named-readtables:in-readtable :interpol-syntax)

(ql:quickload :fiveam)
(setf fiveam:*run-test-when-defined* nil)

(ql:quickload :advent19)
(ql:quickload :advent19/tests)

(log:config :debug)
(log:config :sane2)
(log:config :nofile)

(setf fiveam:*run-test-when-defined* t)

