(defconstant +digits+ (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) "We use this constant for mapping number characters to actual digits.")

(defun getDigitCharToNumberHash()
  "This hash table is used for mapping digit characters to actual numeric digits when converting the string to a number."
  (let ((digitHash (make-hash-table)))
    (setf (gethash #\0 digitHash) 0)
    (setf (gethash #\1 digitHash) 1)
    (setf (gethash #\2 digitHash) 2)
    (setf (gethash #\3 digitHash) 3)
    (setf (gethash #\4 digitHash) 4)
    (setf (gethash #\5 digitHash) 5)
    (setf (gethash #\6 digitHash) 6)
    (setf (gethash #\7 digitHash) 7)
    (setf (gethash #\8 digitHash) 8)
    (setf (gethash #\9 digitHash) 9)
    (return-from getDigitCharToNumberHash digitHash)))

(defun convertStringToInt(str)
  "This function is an alternative to the parse-integer function."
  (defun isStringConvertibleToInteger(str)
    (let ((isInteger t))
      (cond ((= (length str) 0) (setq isInteger nil))                                             ; case 1: no characters
	    ((and (= (length str) 1) (not (member (aref str 0) +digits+))) (setq isInteger nil))    ; case 2: single character, should be numeric
	    ((> (length str) 1)                                                                   ; case 3: more than one character, only first character can be '-' (all others should be numeric)
	     (if (not (or (member (aref str 0) +digits+) (char-equal (aref str 0) #\-)))
		 (setq isInteger nil)
	       (loop for index from 1 to (- (length str) 1)
		     do (unless (member (aref str index) +digits+)
			  (setq isInteger nil)
			  (return))))))
      (return-from isStringConvertibleToInteger isInteger)))
  (check-type str string)
  (let ((intResult))
    (when (isStringConvertibleToInteger str)
      (setq intResult 0)
      (let ((startPos 0) (charToNumberHash (getDigitCharToNumberHash)))
	(when (char-equal (aref str 0) #\-)
	  (setq startPos 1))
	(loop for index from startPos to (- (length str) 1)
	      do
	      (setq intDigitExponent (- (length str) 1 index))
	      (setq intDigit (gethash (aref str index) charToNumberHash))
	      (setq intResult (+ intResult (* intDigit (expt 10 intDigitExponent)))))
	(if (= startPos 1)
	    (setq intResult (- 0 intResult)))))
    (return-from convertStringToInt intResult)))

(defun convertStringToFloat(str)
  "This function parses a string to a float or fraction (depending on string format). It uses a state machine with following states:
  0 - Init, 1 - SignAdded, 2 - LeftDigitsAdded, 3 - CommaAdded, 4 - RightDigitsAdded, 5 - Invalid, 6 - SlashAdded, 7 - SecondSignAdded (last two states for fraction strings only)."
  (check-type str string)
  (let ((commaPosition -1) (slashPosition -1) (isNegative) (charToNumberHash (getDigitCharToNumberHash)) (leftNumberString) (rightNumberString) (result))
    (defun isStringConvertibleToFloat(str)
      (let ((state 0) (isDivisionByZero))
	(cond ((= (length str) 0) (setq state 5))
	      ((char-equal (aref str 0) #\-) (setq state 1) (setq isNegative t))
	      ((member (aref str 0) +digits+) (setq state 2))
	      (t (setq state 5)))
	(when (/= state 5)
	  (dotimes (checkedCharNr (- (length str) 1))
	    (let ((index (+ checkedCharNr 1)))
	      (case state
		    (1 (cond ((member (aref str index) +digits+) (setq state 2))
			     (t (setq state 5))))
		    (2 (cond ((char-equal (aref str index) #\.) (setq state 3) (setq commaPosition index))
			     ((char-equal (aref str index) #\/) (setq state 6) (setq slashPosition index))
			     ((not (member (aref str index) +digits+)) (setq state 5))))
		    (3 (cond ((member (aref str index) +digits+) (setq state 4))
			     (t (setq state 5))))
		    (4 (cond ((not (member (aref str index) +digits+)) (setq state 5))))
		    (5 (return))
		    (6 (cond ((member (aref str index) +digits+) (setq state 4))
			     ((char-equal (aref str index) #\-) (setq state 7) (setq isNegative (not isNegative)))
			     (t (setq state 5))))
		    (7 (cond ((member (aref str index) +digits+) (setq state 4))
			     (t (setq state 5))))))))
        ; retrieve pure numeric substrings (left side and (optionally, if / or . separator exists) right side)
	(when (or (= state 2) (= state 4))
	  (cond ((/= commaPosition -1) ; case 1: decimal string, comma available
		 (setq rightNumberString (subseq str (+ commaPosition 1)))
		 (if (char-equal (aref str 0) #\-)
		     (setq leftNumberString (subseq str 1 commaPosition))
		   (setq leftNumberString (subseq str 0 commaPosition))))
		((/= slashPosition -1) ; case 2: fraction string, slash available
		 (if (char-equal (aref str 0) #\-)
		     (setq leftNumberString (subseq str 1 slashPosition))
		   (setq leftNumberString (subseq str 0 slashPosition)))
		 (if (char-equal (aref str (+ slashPosition 1)) #\-)
		     (setq rightNumberString (subseq str (+ slashPosition 2)))
		   (setq rightNumberString (subseq str (+ slashPosition 1)))))
		((not (null isNegative)) (setq leftNumberString (subseq str 1))) ; case 3a: negative integer
		(t (setq leftNumberString str))) ; case 3b: positive integer
          ; if the string is a fraction it should not be convertible if a division by zero is detected
	  (when (and (= state 4) (/= slashPosition -1) (= (length (string-left-trim "0" rightNumberString)) 0))
	    (setq isDivisionByZero t)))
	(return-from isStringConvertibleToFloat (or (= state 2) (and (= state 4) (not isDivisionByZero)))))) ; the string should be an integer or a float/fraction
    (defun getIntegerFromString(intString) ; converts a (checked) pure-numeric string (no negative sign) to integer, e.g. "123" to 123
      (let ((result 0))
	(loop for index from 0 to (- (length intString) 1)
	      do
	      (let ((digitExponent (- (length intString) 1 index)) (numberDigit (gethash (aref intString index) charToNumberHash)))
		(setq result (+ result (* numberDigit (expt 10 digitExponent))))))
	(return-from getIntegerFromString result)))
    (defun getDecimalFromString(decString) ; convert a (checked) pure-numeric string (no negative sign) to pure decimal, e.g. "123" to 0.123
      (let ((result 0))
	(loop for index from 0 to (- (length decString) 1)
	      do
	      (let ((digitExponent (+ 1 index)) (numberDigit (gethash (aref decString index) charToNumberHash)))
		(setq result (+ result (* numberDigit (expt 0.1 digitExponent))))))
	(return-from getDecimalFromString result)))
    (when (isStringConvertibleToFloat str)
      ; do actual conversion to a float value
      (setq result 0.0)
      (unless (null rightNumberString)
	(if (/= commaPosition -1) ; get right side number (pure decimal part / fraction denominator)
	    (setq result (getDecimalFromString rightNumberString))
	  (setq result (getIntegerFromString rightNumberString))))
      (let ((intPart (getIntegerFromString leftNumberString))) ; get integer part or numerator and combine it to partial outcome to obtain the end result
	(if (/= slashPosition -1)
	    (if (= 0 (rem intPart result))
		(setq result (* 1.0 (/ intPart result))) ; integer fraction
	      (setq result (/ intPart result))) ; non-integer fraction, store as rational number
	  (setq result (+ result intPart)))) ; integer part for integer/decimal string
      (unless (null isNegative) ; finally add the negative sign (if applicable)
	(setq result (- 0 result))))
    (return-from convertStringToFloat result)))
