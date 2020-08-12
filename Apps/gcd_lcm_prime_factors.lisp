(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")
(load "../Libs/parse.lisp")

(defun main()
  (defun getOrderedPrimeFactorsList(number)
    (let ((primeFactorsList (list)) (primeFactorsHash (getPrimeFactorsForNumber number)))
      (if (null primeFactorsHash)
	  (setq primeFactorsList (cons (list (abs number) 1) primeFactorsList))
	(progn
	  (loop for v being the hash-values of primeFactorsHash using (hash-key k)
		do (setq primeFactorsList (cons (list k v) primeFactorsList)))
	  (sort primeFactorsList #'< :key #'cadr)))
      (return-from getOrderedPrimeFactorsList primeFactorsList)))
  (let ((first) (second)) ; declare the variables here for a better overview
    (setq first (requestIntInputWithCondition "Enter the first number: " #'(lambda(val)(setq isDifferent (/= val 0))) "The number should be different from 0. Please try again"))
    (when (not (null first))
      (setq second (requestIntInputWithCondition "Enter the second number: " #'(lambda(val)(setq isDifferent (/= val 0))) "The number should be different from 0. Please try again")))
    (if (not (null second))
	(let ((firstNrPrimeFactors (getOrderedPrimeFactorsList first)) (secondNrPrimeFactors (getOrderedPrimeFactorsList second)) (gcd (gCommonDivPrimeFactors first second)) (lcm (lCommonMulPrimeFactors first second)))
	  (terpri)
	  (format t "Prime factors for first number (absolute values): ~a~%" firstNrPrimeFactors)
	  (format t "Prime factors for second number (absolute values): ~a~%" secondNrPrimeFactors)
	  (terpri)
	  (format t "The greatest common divisor of ~d and ~d is: ~d~%" first second gcd)
          (when (= gcd 1)
	    (write-line "The numbers are prime among each other!"))
	  (format t "The lowest common multiple of ~d and ~d is: ~d~%" first second lcm))
      (write-line "You quit"))))

(main)
