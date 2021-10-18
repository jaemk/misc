(load "deps.lisp")
(ql:quickload :advent19)
(ql:quickload :advent19/tests)
(ql:quickload :fiveam)
(use-package 'arrow-macros)
(use-package 'metabang-bind)
(named-readtables:in-readtable :interpol-syntax)

(log:config :debug)
(log:config :sane2)
(log:config :nofile)

(setf fiveam:*run-test-when-defined* t)

