(defconstant +deps+
  (list
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
    ))

(loop for d in +deps+ do
  (ql:quickload d))

