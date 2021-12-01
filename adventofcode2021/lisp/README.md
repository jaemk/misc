# advent of code 2021

> lisp

```shell
brew install sbcl

make test
make run
```

Or from a repl

```lisp
(load "dev.lisp")

;; executing the definition of any test will execute the test.
;; "loading" a test file will execute all of its tests.
;; "loading" the tests package will execute all tests similar
;; to `make test`
(ql:quickload :advent/tests)
```

