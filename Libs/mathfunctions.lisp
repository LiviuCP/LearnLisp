(defconstant +first-relevant-prime+ 2 "First useful prime number (1 is not relevant).")

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
  (defun get-gcd-one-prime-number(prime-number not-prime-number)
    (let ((greatest-common-divisor-one-prime 1))
      (if (= (rem not-prime-number prime-number) 0)
	  (setq greatest-common-divisor-one-prime prime-number))
      (return-from get-gcd-one-prime-number greatest-common-divisor-one-prime)))
  ; simplify by using absolute values for retrieving l.c.m.
  (let* ((greatest-common-divisor 1)
	 (absolute-first (abs first))
	 (absolute-second (abs second))
	 (prime-factors-first (get-prime-factors-for-number absolute-first))
	 (prime-factors-second (get-prime-factors-for-number absolute-second)))
    (cond ((and (not (null prime-factors-first)) (not (null prime-factors-second)))
	   (loop for prime-factor being each hash-key of prime-factors-first ; get common prime factors for both numbers and use the minimum exponent to calculate g.c.d.
		 do (unless (null (gethash prime-factor prime-factors-second))
		      (let ((first-exponent (gethash prime-factor prime-factors-first)) (second-exponent (gethash prime-factor prime-factors-second)))
			(setq greatest-common-divisor (* greatest-common-divisor (expt prime-factor (min first-exponent second-exponent))))))))
	  ((and (not (null prime-factors-first)) (null prime-factors-second) (> absolute-first absolute-second)) (setq greatest-common-divisor (get-gcd-one-prime-number absolute-second absolute-first))) ; second number is prime, no need to use prime factors
	  ((and (not (null prime-factors-second)) (null prime-factors-first) (> absolute-second absolute-first)) (setq greatest-common-divisor (get-gcd-one-prime-number absolute-first absolute-second))) ; first number is prime, no need to use prime factors
	  ((= absolute-first absolute-second) (setq greatest-common-divisor absolute-first)))
    (return-from greatest-common-divisor-prime-factors greatest-common-divisor)))

(defun least-common-multiple-prime-factors(first second)
  "This function retrieves the least common multiple of two integer numbers by using the prime factors decomposition."
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (defun get-least-common-multiple-one-prime-number(prime-number not-prime-number)
    (let ((least-common-multiple-one-prime 1))
      (if (= (rem not-prime-number prime-number) 0)
	  (setq least-common-multiple-one-prime (abs not-prime-number))
	(setq least-common-multiple-one-prime (* prime-number not-prime-number)))
    (return-from get-least-common-multiple-one-prime-number least-common-multiple-one-prime)))
  (let* ((least-common-multiple 1)
	 (absolute-first (abs first))
	 (absolute-second (abs second))
	 (prime-factors-first (get-prime-factors-for-number absolute-first))
	 (prime-factors-second (get-prime-factors-for-number absolute-second)))
    (cond ((and (not (null prime-factors-first)) (not (null prime-factors-second)))
	   (let ((consolidated-prime-factors prime-factors-second))
	     (loop for prime-factor being each hash-key of prime-factors-first ; get common prime factors for both numbers and use the maximum exponent to calculate l.c.m.
		   do
		   (let ((resulting-exponent (gethash prime-factor prime-factors-first)))
		     (unless (null (gethash prime-factor prime-factors-second))
		       (setq resulting-exponent (max resulting-exponent (gethash prime-factor prime-factors-second))))
		     (setf (gethash prime-factor consolidated-prime-factors) resulting-exponent)))
	     (loop for prime-factor being each hash-key of consolidated-prime-factors ; calculate l.c.m. based on consolidated prime factors
		   do
		   (setq least-common-multiple (* least-common-multiple (expt prime-factor (gethash prime-factor consolidated-prime-factors)))))))
	  ((and (not (null prime-factors-first)) (null prime-factors-second) (> absolute-first absolute-second)) (setq least-common-multiple (get-least-common-multiple-one-prime-number absolute-second absolute-first))) ; second number is prime, no need to use prime factors
	  ((and (not (null prime-factors-second)) (null prime-factors-first) (> absolute-second absolute-first)) (setq least-common-multiple (get-least-common-multiple-one-prime-number absolute-first absolute-second))) ; first number is prime, no need to use prime factors
	  ((= absolute-first absolute-second) (setq least-common-multiple absolute-first))
	  (t (setq least-common-multiple (* absolute-first absolute-second))))
    (return-from least-common-multiple-prime-factors least-common-multiple)))

(defun get-prime-factors-for-number(number)
  "This function retrieves prime factors for an integer number."
  (check-type number integer)
  (assert (/= number 0) (number) "The argument should be different from 0")
  (defun convert-primes-array-to-list(primes-array)
    (let ((array-length (length primes-array)) (primes-list (list)))
      (dotimes (index array-length)
	(setq primes-list (cons (aref primes-array (- (- array-length 1) index)) primes-list)))
      (return-from convert-primes-array-to-list primes-list)))
  (let ((result) (number-to-divide (abs number))) ; key = prime number, value = number of occurrences within the number-to-divide (power exponent)
    (unless (= number-to-divide 1)
      (let ((primes-until-number (convert-primes-array-to-list (get-prime-numbers number-to-divide))))
	(unless (member number-to-divide primes-until-number)
	  (let ((prime-factors (make-hash-table)))
	    (loop for prime in primes-until-number
		  do (when (= (rem number-to-divide prime) 0)
		       (let ((occurrences 1))
			 (setq number-to-divide (floor number-to-divide prime))
			 (loop
			  (when (/= (rem number-to-divide prime) 0)
			    (return))
			  (setq number-to-divide (floor number-to-divide prime))
			  (incf occurrences 1))
			 (setf (gethash prime prime-factors) occurrences)))
		       (when (= number-to-divide 1)
			   (return)))
	    (setq result prime-factors)))))
    (return-from get-prime-factors-for-number result)))

(defun get-prime-numbers(right &optional left) ; the search interval has only one mandatory defined margin, namely the right one (left is optional, if not defined than it is presumed 2 - first relevant prime nr)
  "This function retrieves prime numbers in a specific interval (default is [2; right])."
  (check-type right integer)
  (assert (> right 1) (right) "The given threshold is invalid")
  (unless (null left)
    (check-type left integer)
    (assert (and (> left 1) (> right left)) (left right) "The given interval is invalid"))
  ; initial allocation of 10% of all numbers belonging to interval (adjust if required), min 2 elements (2, 3) required
  (let* ((initial-primes-array-capacity (+ (floor right 10) 2)) (identified-primes (make-array `(,initial-primes-array-capacity) :fill-pointer 2 :adjustable t)) (identified-primes-final))
    (setf (aref identified-primes 0) 2)                         ; seed the list of prime numbers (first number not to be used for checking as it is the only even one)
    (setf (aref identified-primes 1) 3)
    (do ((current-number-to-check 5 (+ current-number-to-check 2)))          ; only check odd numbers for prime-ness
	((> current-number-to-check right))
	(dotimes (index (- (fill-pointer identified-primes) 1)) ; exclude 2 from primes number check base
	  (let* ((current-prime-number (aref identified-primes (+ index 1))) (quotient (floor current-number-to-check current-prime-number)) (remainder (rem current-number-to-check current-prime-number)))
	    (when (= 0 remainder)
	      (return))
	    (when (< quotient current-prime-number)                    ; add found prime to list of already existing ones (use it in the next iterations for finding new primes)
	      (vector-push-extend current-number-to-check identified-primes)
	      (return)))))
    (when (= right 2)                                    ; corner-case, user enters only number 2 as interval (clip everything else)
      (vector-pop identified-primes))
    (let ((left-index 0))
      (unless (null left)                              ; adjust array to contain only numbers from interval
	(dotimes (index (fill-pointer identified-primes))
	  (if (< (aref identified-primes index) left)
	      (incf left-index 1)
	    (return))))
      (let ((final-number-of-elements (- (fill-pointer identified-primes) left-index)))
	(setq identified-primes-final (make-array `(,final-number-of-elements) :displaced-to identified-primes :displaced-index-offset left-index))))
    (return-from get-prime-numbers identified-primes-final)))

(defun get-prime-numbers-eratostene(right &optional left) ; the search interval has only one mandatory defined margin, namely the right one (left is optional, if not defined than it is presumed 2 - first relevant prime nr)
  "This function retrieves prime numbers in a specific interval (default is [2; right]) by using the algorithm or Eratostene."
  (check-type right integer)
  (assert (> right 1) (right) "The given threshold is invalid")
  (unless (null left)
    (check-type left integer)
    (assert (and (> left 1) (> right left)) (left right) "The given interval is invalid"))
  (let* ((initial-primes-array-capacity (+ (floor right 10) 1)) ; initial allocation of 10% of all numbers belonging to interval (adjust if required), min 1 element required
	 (identified-primes (make-array `(,initial-primes-array-capacity) :fill-pointer 0 :adjustable t))
	 (identified-primes-final)
	 (checked-array-size (+ right 1)) ; include 0 in the array of checked numbers to avoid unnecessary arithmetic operations index conversion into number (index = number)
	 (all-checked-numbers (make-array `(,checked-array-size) :adjustable t :initial-element t)) ; all checked numbers initially considered prime (value t)
	 (check-threshold (sqrt right)) ; all numbers until this threshold are checked for prime-ness and the found prime numbers are used for ruling out multiples from interval
	 (current-found-prime 1)) ; mark 1 as "special number" (actually neither considered prime nor "not prime" -> this is the starting point for checking the range of numbers for prime-ness)
    ; step 1: find all prime numbers until threshold and mark all multiples as non-prime
    (loop
     (do ((current-number-to-check (+ current-found-prime 1) (+ current-number-to-check 1)))
	 ((not (null (aref all-checked-numbers current-number-to-check)))(setq current-found-prime current-number-to-check)))
     (vector-push-extend current-found-prime identified-primes)
     (if (> current-found-prime check-threshold)
	 (return)
       (do ((multiplication-factor 2 (+ multiplication-factor 1))) ; multiples of 2, 3, ... of the found prime to be marked as not prime (nil) in the checked numbers array
	   ((> (* multiplication-factor current-found-prime) right))
	   (setf (aref all-checked-numbers (* multiplication-factor current-found-prime)) nil))))
    ; step 2 : go beyond threshold, gather all numbers that are still marked as prime and consolidate them with the already found primes
    (do ((current-number-to-check (+ current-found-prime 1) (+ current-number-to-check 1)))
	((> current-number-to-check right))
	(unless (null (aref all-checked-numbers current-number-to-check))
	  (vector-push-extend current-number-to-check identified-primes)))
    ; step 3 : clip the left part of the interval if required
    (let ((left-index 0))
      (unless (null left)
	(dotimes (index (fill-pointer identified-primes))
	  (if (< (aref identified-primes index) left)
	      (incf left-index 1)
	    (return))))
      (let ((final-number-of-elements (- (fill-pointer identified-primes) left-index)))
	(setq identified-primes-final (make-array `(,final-number-of-elements) :displaced-to identified-primes :displaced-index-offset left-index))))
    (return-from get-prime-numbers-eratostene identified-primes-final)))

(defun get-fibonacci-matrix(order &optional initial-values) ; initial-values should be a list of four integer elements that are (in this order): (0, 0), (0, 1), (1, 0), (1, 1)
  "This function calculates a Fibonacci-like matrix; on each row, column and diagonal each number is the sum of the previous two numbers (initial numbers can be provided by user)."
  (check-type order integer)
  (assert (> order 0) (order) "The matrix order is invalid")
  (unless (null initial-values)
    (check-type initial-values list)
    (assert (>= (length initial-values) 4) (initial-values) "The initial values list contains less than the required four elements")
    (dotimes (index 4)
      (check-type (nth index initial-values) integer))
    (assert (>= (nth 0 initial-values) 0) (initial-values) "Negative initial value identified")
    (dotimes (index 3)
      (assert (> (nth (+ index 1) initial-values) (nth 0 initial-values)) (index initial-values) "First matrix element (0, 0) is not smaller than each of the other three: (0, 1), (1, 0), (1, 1)")))
  (let ((fibonacci-matrix (make-array `(,order ,order))))
    (if (not (null initial-values))
	(setf (aref fibonacci-matrix 0 0) (nth 0 initial-values))
      (setf (aref fibonacci-matrix 0 0) 0))
    (when (> order 1)
      (cond ((not (null initial-values))
	     (setf (aref fibonacci-matrix 0 1) (nth 1 initial-values))
	     (setf (aref fibonacci-matrix 1 0) (nth 2 initial-values))
	     (setf (aref fibonacci-matrix 1 1) (nth 3 initial-values)))
	    (t
	     (setf (aref fibonacci-matrix 0 1) 1)
	     (setf (aref fibonacci-matrix 1 0) 1)
	     (setf (aref fibonacci-matrix 1 1) 1))))
    (when (> order 2)
      (loop for diagonal-index from 2 to (- order 1)
	    do
	    (let ((previous-diagonal-index (- diagonal-index 1)) (second-previous-diagonal-index (- diagonal-index 2)))
	      (setf (aref fibonacci-matrix diagonal-index diagonal-index) (+ (aref fibonacci-matrix second-previous-diagonal-index second-previous-diagonal-index) (aref fibonacci-matrix previous-diagonal-index previous-diagonal-index)))
	      (dotimes (row-index diagonal-index)
		(setf (aref fibonacci-matrix row-index diagonal-index) (+ (aref fibonacci-matrix row-index second-previous-diagonal-index) (aref fibonacci-matrix row-index previous-diagonal-index))))
	      (dotimes (column-index diagonal-index)
		(setf (aref fibonacci-matrix diagonal-index column-index) (+ (aref fibonacci-matrix second-previous-diagonal-index column-index) (aref fibonacci-matrix previous-diagonal-index column-index)))))))
    (return-from get-fibonacci-matrix fibonacci-matrix)))
