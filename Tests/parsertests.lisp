(load "../Libs/parse.lisp")

(define-test test-is-string-not-integer
  (assert-false (isStringInteger "a"))
  (assert-false (isStringInteger "-1b"))
  (assert-false (isStringInteger "1 2"))
  (assert-false (isStringInteger ""))
  (assert-false (isStringInteger ".12"))
  (assert-false (isStringInteger " 1"))
  (assert-false (isStringInteger "-2 "))
  (assert-false (isStringInteger "1+2"))
  (assert-false (isStringInteger "1-23")))

(define-test test-is-string-integer
  (assert-true (isStringInteger "0"))
  (assert-true (isStringInteger "00"))
  (assert-true (isStringInteger "-12"))
  (assert-true (isStringInteger "45"))
  (assert-true (isStringInteger "0014"))
  (assert-true (isStringInteger "-0015")))

