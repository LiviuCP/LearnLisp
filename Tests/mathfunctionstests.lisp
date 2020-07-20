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

(define-test test-greatest-common-div-prime-factors
  (assert-equal (gCommonDivPrimeFactors 6 10) 2)
  (assert-equal (gCommonDivPrimeFactors -10 -6) 2)
  (assert-equal (gCommonDivPrimeFactors 20 8) 4)
  (assert-equal (gCommonDivPrimeFactors -8 20) 4)
  (assert-equal (gCommonDivPrimeFactors 15 32) 1)
  (assert-equal (gCommonDivPrimeFactors 4 4) 4)
  (assert-equal (gCommonDivPrimeFactors -1 1) 1)
  (assert-equal (gCommonDivPrimeFactors 17 19) 1)
  (assert-equal (gCommonDivPrimeFactors -30 15) 15)
)

(define-test test-prime-numbers-in-interval
  (assert-true (equalp (getPrimeNumbers 2) #(2)))
  (assert-true (equalp (getPrimeNumbers 10) #(2 3 5 7)))
  (assert-true (equalp (getPrimeNumbers 20 2) #(2 3 5 7 11 13 17 19)))
  (assert-true (equalp (getPrimeNumbers 16 14) #()))
  (assert-true (equalp (getPrimeNumbers 50 20) #(23 29 31 37 41 43 47)))
  (assert-true (equalp (getPrimeNumbers 17 3) #(3 5 7 11 13 17)))
  (assert-true (equalp (getPrimeNumbers 17) #(2 3 5 7 11 13 17)))
  (assert-true (equalp (getPrimeNumbers 20 11) #(11 13 17 19)))
)

(define-test test-prime-factors
  (assert-false (getPrimeFactorsForNumber 1))
  (assert-false (getPrimeFactorsForNumber 2))
  (assert-false (getPrimeFactorsForNumber 7))

  (setq primeFactors (getPrimeFactorsForNumber 6))
  (assert-true (= (gethash 2 primeFactors) 1)) ; assert-equal doesn't seem to work in these cases
  (assert-true (= (gethash 3 primeFactors) 1))

  (setq primeFactors (getPrimeFactorsForNumber 384))
  (assert-true (= (gethash 2 primeFactors) 7))
  (assert-true (= (gethash 3 primeFactors) 1))

  (setq primeFactors (getPrimeFactorsForNumber 2500))
  (assert-true (= (gethash 2 primeFactors) 2))
  (assert-true (= (gethash 5 primeFactors) 4))
)
