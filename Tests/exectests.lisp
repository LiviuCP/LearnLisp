(load "../../lisp-unit/lisp-unit.lisp")
(in-package :lisp-unit)

(load "parsertests.lisp")
(load "mathfunctionstests.lisp")

(run-tests :all)
