(defconstant firstRelevantPrime 2)

; calculates the greatest common divisor by remainder method
(defun gCommonDiv (first second)
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (setq result 1)
  (setq divided first)
  (setq divider second)
  (loop
   (setq remainder (rem divided divider))
   (cond ((= remainder 0)(setq result divider)(return))
	 (t (setq divided divider) (setq divider remainder))))
  (return-from gCommonDiv result))

(defun getPrimeNumbers(right &optional left) ; the search interval has only one mandatory defined margin, namely the right one (left is optional, if not defined than it is presumed 2 - first relevant prime nr)
  (check-type right integer)
  (assert (> right 1) (right) "The given threshold is invalid")
  (when (not (null left))
    (check-type left integer)
    (assert (and (> left 1) (> right left)) (left right) "The given interval is invalid"))
  (setq initialPrimesArrayCapacity (+ (floor right 10) 2)) ; initial allocation of 10% of all numbers belonging to interval (adjust if required), min 2 elements (2, 3) required
  (setq identifiedPrimes (make-array `(,initialPrimesArrayCapacity) :fill-pointer 2 :adjustable t))
  (setf (aref identifiedPrimes 0) 2)                         ; seed the list of prime numbers (first number not to be used for checking as it is the only even one)
  (setf (aref identifiedPrimes 1) 3)
  (do ((currentNrToCheck 5 (+ currentNrToCheck 2)))          ; only check odd numbers for prime-ness
      ((> currentNrToCheck right))
      (dotimes (index (- (fill-pointer identifiedPrimes) 1)) ; exclude 2 from primes number check base
	(setq currentPrimeNr (aref identifiedPrimes (+ index 1)))
	(setq quotient (floor currentNrToCheck currentPrimeNr))
	(setq remainder (rem currentNrToCheck currentPrimeNr))
	(when (= 0 remainder)
	  (return))
	(when (< quotient currentPrimeNr)                    ; add found prime to list of already existing ones (use it in the next iterations for finding new primes)
	  (when (= (fill-pointer identifiedPrimes) (length identifiedPrimes))
	    (setq adjustSize (* 2 (length identifiedPrimes)))
	    (adjust-array identifiedPrimes `(,adjustSize)))
	  (vector-push currentNrToCheck identifiedPrimes)
	  (return))))
  (when (= right 2)                                    ; corner-case, user enters only number 2 as interval (clip everything else)
    (vector-pop identifiedPrimes))
  (setq leftIndex 0)
  (when (not (null left))                              ; adjust array to contain only numbers from interval
    (dotimes (index (fill-pointer identifiedPrimes))
      (if (< (aref identifiedPrimes index) left)
	  (incf leftIndex 1)
	(return))))
  (setq finalNrOfElements (- (fill-pointer identifiedPrimes) leftIndex))
  (setq identifiedPrimesFinal (make-array `(,finalNrOfElements) :displaced-to identifiedPrimes :displaced-index-offset leftIndex))
  (return-from getPrimeNumbers identifiedPrimesFinal))
