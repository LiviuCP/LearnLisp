(load "../Libs/parse.lisp")

(defconstant epsilon (expt 10 -6))

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

(define-test test-convert-to-float
  (assert-true (<= (abs (- 2.345678000001 (convertStringToFloat "2.345678000001"))) epsilon))
  (assert-true (<= (abs (- -1.8954 (convertStringToFloat "-1.8954"))) epsilon))
  (assert-true (<= (abs (- 12.0 (convertStringToFloat "12"))) epsilon))
  (assert-true (<= (abs (- -4.0 (convertStringToFloat "-4"))) epsilon))
  (assert-true (<= (abs (- 0.0 (convertStringToFloat "0"))) epsilon))
  (assert-true (<= (abs (- 0.0 (convertStringToFloat "0.0"))) epsilon))
  (assert-true (<= (abs (- 0.0 (convertStringToFloat "-0"))) epsilon))
  (assert-true (<= (abs (- 0.0 (convertStringToFloat "-0.00"))) epsilon))
  (assert-true (<= (abs (- 11.0 (convertStringToFloat "11.000"))) epsilon))
  (assert-true (<= (abs (- 1.2354 (convertStringToFloat "01.2354"))) epsilon))
  (assert-true (<= (abs (- -2.4501 (convertStringToFloat "-02.450100"))) epsilon)))

(define-test test-cannot-convert-to-float
  (assert-false (convertStringToFloat "a.5"))
  (assert-false (convertStringToFloat "-5.a"))
  (assert-false (convertStringToFloat "b"))
  (assert-false (convertStringToFloat "1.5.2"))
  (assert-false (convertStringToFloat "-.5"))
  (assert-false (convertStringToFloat "-5-"))
  (assert-false (convertStringToFloat " 5.2"))
  (assert-false (convertStringToFloat "-5. 25"))
  (assert-false (convertStringToFloat "2.25 "))
  (assert-false (convertStringToFloat ""))
  (assert-false (convertStringToFloat " ")))
