(load "../Libs/mathfunctions.lisp")

(define-test test-greatest-common-div
  (assert-equal (gCommonDiv 6 10) 2)
  (assert-equal (gCommonDiv -10 -6) 2)
  (assert-equal (gCommonDiv 20 8) 4)
  (assert-equal (gCommonDiv -8 20) 4)
  (assert-equal (gCommonDiv 15 32) 1)
  (assert-equal (gCommonDiv 4 4) 4)
  (assert-equal (gCommonDiv -1 1) 1)
  (assert-equal (gCommonDiv 17 19) 1)
  (assert-equal (gCommonDiv -30 15) 15)
)
