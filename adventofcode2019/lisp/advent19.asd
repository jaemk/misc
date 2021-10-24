(defsystem "advent19"
  :version "0.0.0"
  :author "James Kominick"
  :license "MIT"
  :depends-on (
      "uuid"
      "ironclad"
      "arrow-macros"
      "log4cl"
      "str"
      "hunchentoot"
      "drakma"
      "cl-json"
      "metabang-bind"
      "cl-interpol"
      "cl-ppcre"
      "local-time"
      "trivial-backtrace"
      "bordeaux-threads"
      "uiop"
      "alexandria"
      "chanl"
      "cl-permutation"
  )
  :serial t
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "config")
                 (:file "vm")
                 (:file "d01")
                 (:file "d02")
                 (:file "d03")
                 (:file "d04")
                 (:file "d05")
                 (:file "d06")
                 (:file "d07")
                 (:file "d08")
                 (:file "d09")
                 (:file "main")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "advent19/tests"))))

(defsystem "advent19/tests"
  :author "James Kominick"
  :license "MIT"
  :depends-on ("advent19"
               "fiveam")
  :serial t
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "test-d01")
                 (:file "test-d02")
                 (:file "test-d03")
                 (:file "test-d04")
                 (:file "test-d05")
                 (:file "test-d06")
                 (:file "test-d07")
                 (:file "test-d08")
                 (:file "test-d09")
                 )))
  :description "Test system for advent19"
  :perform (test-op (op sys)
             (symbol-call
               :fiveam :run! (find-symbol* :all :advent19/tests))))

