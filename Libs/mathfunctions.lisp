(defconstant +firstRelevantPrime+ 2 "First useful prime number (1 is not relevant).")

(defun greatest-common-divisor (first second)
  "This function retrieves the greatest common divisor of two integer numbers by using the remainder method."
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (let ((result 1) (divided (abs first)) (divider (abs second))) ; ensure the g.c.d. is positive to avoid any confusion
    (loop
     (let ((remainder (rem divided divider)))
       (cond ((= remainder 0)(setq result divider)(return))
	     (t (setq divided divider) (setq divider remainder)))))
    (return-from greatest-common-divisor result)))

(defun greatest-common-divisor-prime-factors(first second)
  "This function retrieves the greatest common divisor of two integer numbers by using the prime factors decomposition."
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (defun get-gcd-one-prime-number(primeNr notPrimeNr)
    (let ((gcdOnePrime 1))
      (if (= (rem notPrimeNr primeNr) 0)
	  (setq gcdOnePrime primeNr))
      (return-from get-gcd-one-prime-number gcdOnePrime)))
  ; simplify by using absolute values for retrieving l.c.m.
  (let* ((gcd 1) (absFirst (abs first)) (absSecond (abs second)) (primeFactorsFirst (get-prime-factors-for-number absFirst)) (primeFactorsSecond (get-prime-factors-for-number absSecond)))
    (cond ((and (not (null primeFactorsFirst)) (not (null primeFactorsSecond)))
	   (loop for primeFactor being each hash-key of primeFactorsFirst ; get common prime factors for both numbers and use the minimum exponent to calculate g.c.d.
		 do (unless (null (gethash primeFactor primeFactorsSecond))
		      (let ((firstExponent (gethash primeFactor primeFactorsFirst)) (secondExponent (gethash primeFactor primeFactorsSecond)))
			(setq gcd (* gcd (expt primeFactor (min firstExponent secondExponent))))))))
	  ((and (not (null primeFactorsFirst)) (null primeFactorsSecond) (> absFirst absSecond)) (setq gcd (get-gcd-one-prime-number absSecond absFirst))) ; second number is prime, no need to use prime factors
	  ((and (not (null primeFactorsSecond)) (null primeFactorsFirst) (> absSecond absFirst)) (setq gcd (get-gcd-one-prime-number absFirst absSecond))) ; first number is prime, no need to use prime factors
	  ((= absFirst absSecond) (setq gcd absFirst)))
    (return-from greatest-common-divisor-prime-factors gcd)))

(defun least-common-multiple-prime-factors(first second)
  "This function retrieves the least common multiple of two integer numbers by using the prime factors decomposition."
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (defun get-lcm-one-prime-number(primeNr notPrimeNr)
    (let ((lcmOnePrime 1))
      (if (= (rem notPrimeNr primeNr) 0)
	  (setq lcmOnePrime (abs notPrimeNr))
	(setq lcmOnePrime (* primeNr notPrimeNr)))
    (return-from get-lcm-one-prime-number lcmOnePrime)))
  (let* ((lcm 1) (absFirst (abs first)) (absSecond (abs second)) (primeFactorsFirst (get-prime-factors-for-number absFirst)) (primeFactorsSecond (get-prime-factors-for-number absSecond)))
    (cond ((and (not (null primeFactorsFirst)) (not (null primeFactorsSecond)))
	   (let ((consolidatedPrimeFactors primeFactorsSecond))
	     (loop for primeFactor being each hash-key of primeFactorsFirst ; get common prime factors for both numbers and use the maximum exponent to calculate l.c.m.
		   do
		   (let ((resultingExponent (gethash primeFactor primeFactorsFirst)))
		     (unless (null (gethash primeFactor primeFactorsSecond))
		       (setq resultingExponent (max resultingExponent (gethash primeFactor primeFactorsSecond))))
		     (setf (gethash primeFactor consolidatedPrimeFactors) resultingExponent)))
	     (loop for primeFactor being each hash-key of consolidatedPrimeFactors ; calculate l.c.m. based on consolidated prime factors
		   do
		   (setq lcm (* lcm (expt primeFactor (gethash primeFactor consolidatedPrimeFactors)))))))
	  ((and (not (null primeFactorsFirst)) (null primeFactorsSecond) (> absFirst absSecond)) (setq lcm (get-lcm-one-prime-number absSecond absFirst))) ; second number is prime, no need to use prime factors
	  ((and (not (null primeFactorsSecond)) (null primeFactorsFirst) (> absSecond absFirst)) (setq lcm (get-lcm-one-prime-number absFirst absSecond))) ; first number is prime, no need to use prime factors
	  ((= absFirst absSecond) (setq lcm absFirst))
	  (t (setq lcm (* absFirst absSecond))))
    (return-from least-common-multiple-prime-factors lcm)))

(defun get-prime-factors-for-number(number)
  "This function retrieves prime factors for an integer number."
  (check-type number integer)
  (assert (/= number 0) (number) "The argument should be different from 0")
  (defun convert-primes-array-to-list(primesArray)
    (let ((arrayLength (length primesArray)) (primesList (list)))
      (dotimes (index arrayLength)
	(setq primesList (cons (aref primesArray (- (- arrayLength 1) index)) primesList)))
      (return-from convert-primes-array-to-list primesList)))
  (let ((result) (numberToDivide (abs number))) ; key = prime number, value = number of occurrences within the numberToDivide (power exponent)
    (unless (= numberToDivide 1)
      (let ((primesUntilNumber (convert-primes-array-to-list (get-prime-numbers numberToDivide))))
	(unless (member numberToDivide primesUntilNumber)
	  (let ((primeFactors (make-hash-table)))
	    (loop for prime in primesUntilNumber
		  do (when (= (rem numberToDivide prime) 0)
		       (let ((occurrences 1))
			 (setq numberToDivide (floor numberToDivide prime))
			 (loop
			  (when (/= (rem numberToDivide prime) 0)
			    (return))
			  (setq numberToDivide (floor numberToDivide prime))
			  (incf occurrences 1))
			 (setf (gethash prime primeFactors) occurrences)))
		       (when (= numberToDivide 1)
			   (return)))
	    (setq result primeFactors)))))
    (return-from get-prime-factors-for-number result)))

(defun get-prime-numbers(right &optional left) ; the search interval has only one mandatory defined margin, namely the right one (left is optional, if not defined than it is presumed 2 - first relevant prime nr)
  "This function retrieves prime numbers in a specific interval (default is [2; right])."
  (check-type right integer)
  (assert (> right 1) (right) "The given threshold is invalid")
  (unless (null left)
    (check-type left integer)
    (assert (and (> left 1) (> right left)) (left right) "The given interval is invalid"))
  ; initial allocation of 10% of all numbers belonging to interval (adjust if required), min 2 elements (2, 3) required
  (let* ((initialPrimesArrayCapacity (+ (floor right 10) 2)) (identifiedPrimes (make-array `(,initialPrimesArrayCapacity) :fill-pointer 2 :adjustable t)) (identifiedPrimesFinal))
    (setf (aref identifiedPrimes 0) 2)                         ; seed the list of prime numbers (first number not to be used for checking as it is the only even one)
    (setf (aref identifiedPrimes 1) 3)
    (do ((currentNrToCheck 5 (+ currentNrToCheck 2)))          ; only check odd numbers for prime-ness
	((> currentNrToCheck right))
	(dotimes (index (- (fill-pointer identifiedPrimes) 1)) ; exclude 2 from primes number check base
	  (let* ((currentPrimeNr (aref identifiedPrimes (+ index 1))) (quotient (floor currentNrToCheck currentPrimeNr)) (remainder (rem currentNrToCheck currentPrimeNr)))
	    (when (= 0 remainder)
	      (return))
	    (when (< quotient currentPrimeNr)                    ; add found prime to list of already existing ones (use it in the next iterations for finding new primes)
	      (when (= (fill-pointer identifiedPrimes) (length identifiedPrimes))
		(let ((adjustSize (* 2 (length identifiedPrimes))))
		  (adjust-array identifiedPrimes `(,adjustSize)))
	      (vector-push currentNrToCheck identifiedPrimes)
	      (return))))))
    (when (= right 2)                                    ; corner-case, user enters only number 2 as interval (clip everything else)
      (vector-pop identifiedPrimes))
    (let ((leftIndex 0))
      (unless (null left)                              ; adjust array to contain only numbers from interval
	(dotimes (index (fill-pointer identifiedPrimes))
	  (if (< (aref identifiedPrimes index) left)
	      (incf leftIndex 1)
	    (return))))
      (let ((finalNrOfElements (- (fill-pointer identifiedPrimes) leftIndex)))
	(setq identifiedPrimesFinal (make-array `(,finalNrOfElements) :displaced-to identifiedPrimes :displaced-index-offset leftIndex))))
    (return-from get-prime-numbers identifiedPrimesFinal)))

(defun get-fibonacci-matrix(order &optional initialValues) ; initialValues should be a list of four integer elements that are (in this order): (0, 0), (0, 1), (1, 0), (1, 1)
  "This function calculates a Fibonacci-like matrix; on each row, column and diagonal each number is the sum of the previous two numbers (initial numbers can be provided by user)."
  (check-type order integer)
  (assert (> order 0) (order) "The matrix order is invalid")
  (unless (null initialValues)
    (check-type initialValues list)
    (assert (>= (length initialValues) 4) (initialValues) "The initial values list contains less than the required four elements")
    (dotimes (index 4)
      (check-type (nth index initialValues) integer))
    (assert (>= (nth 0 initialValues) 0) (initialValues) "Negative initial value identified")
    (dotimes (index 3)
      (assert (> (nth (+ index 1) initialValues) (nth 0 initialValues)) (index initialValues) "First matrix element (0, 0) is not smaller than each of the other three: (0, 1), (1, 0), (1, 1)")))
  (let ((fibMatrix (make-array `(,order ,order))))
    (if (not (null initialValues))
	(setf (aref fibMatrix 0 0) (nth 0 initialValues))
      (setf (aref fibMatrix 0 0) 0))
    (when (> order 1)
      (cond ((not (null initialValues))
	     (setf (aref fibMatrix 0 1) (nth 1 initialValues))
	     (setf (aref fibMatrix 1 0) (nth 2 initialValues))
	     (setf (aref fibMatrix 1 1) (nth 3 initialValues)))
	    (t
	     (setf (aref fibMatrix 0 1) 1)
	     (setf (aref fibMatrix 1 0) 1)
	     (setf (aref fibMatrix 1 1) 1))))
    (when (> order 2)
      (loop for diagIndex from 2 to (- order 1)
	    do
	    (let ((prevDiagIndex (- diagIndex 1)) (secPrevDiagIndex (- diagIndex 2)))
	      (setf (aref fibMatrix diagIndex diagIndex) (+ (aref fibMatrix secPrevDiagIndex secPrevDiagIndex) (aref fibMatrix prevDiagIndex prevDiagIndex)))
	      (dotimes (rowIndex diagIndex)
		(setf (aref fibMatrix rowIndex diagIndex) (+ (aref fibMatrix rowIndex secPrevDiagIndex) (aref fibMatrix rowIndex prevDiagIndex))))
	      (dotimes (colIndex diagIndex)
		(setf (aref fibMatrix diagIndex colIndex) (+ (aref fibMatrix secPrevDiagIndex colIndex) (aref fibMatrix prevDiagIndex colIndex)))))))
    (return-from get-fibonacci-matrix fibMatrix)))
