(load "../../lisp-unit/lisp-unit.lisp")
(in-package :lisp-unit)

(load "parsertests.lisp")
(load "mathfunctionstests.lisp")
(load "sorttests.lisp")

(run-tests :all)
