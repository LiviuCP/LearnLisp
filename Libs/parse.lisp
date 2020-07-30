(defun getDigitCharToNumberHash()
    (setq digitHash (make-hash-table))
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
    (return-from getDigitCharToNumberHash digitHash))

(defun isStringInteger(str)
  (check-type str string)
  (setq digits (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (setq isInteger t)
  (cond ((= (length str) 0) (setq isInteger nil))                                             ; case 1: no characters
	((and (= (length str) 1) (not (member (aref str 0) digits))) (setq isInteger nil))    ; case 2: single character, should be numeric
	((> (length str) 1)                                                                   ; case 3: more than one character, only first character can be '-' (all others should be numeric)
	 (if (not (or (member (aref str 0) digits) (char-equal (aref str 0) #\-)))
	      (setq isInteger nil)
	    (loop for index from 1 to (- (length str) 1)
		  do (when (not (member (aref str index) digits))
		       (setq isInteger nil)
		       (return))))))
  (return-from isStringInteger isInteger))

; own implementation of parse-integer
(defun convertStringToInt(str)
  (check-type str string)
  (setq intResult nil)
  (when (isStringInteger str)
    (setq intResult 0)
    (setq startPos 0)
    (setq charToNumberHash (getDigitCharToNumberHash))
    (when (char-equal (aref str 0) #\-)
	(setq startPos 1))
    (loop for index from startPos to (- (length str) 1)
	  do
	  (setq intDigitExponent (- (length str) 1 index))
	  (setq intDigit (gethash (aref str index) charToNumberHash))
	  (setq intResult (+ intResult (* intDigit (expt 10 intDigitExponent)))))
    (if (= startPos 1)
	(setq intResult (- 0 intResult))))
  (return-from convertStringToInt intResult))

(defun convertStringToFloat(str)
  ; state-machine with states: 0 - Init, 1 - SignAdded, 2 - LeftDigitsAdded, 3 - CommaAdded, 4 - RightDigitsAdded, 5 - Invalid (possibly to be revised as use of "magic numbers" is not the best solution)
  (check-type str string)
  (setq commaPosition -1)
  (setq isNegative nil)
  (defun isStringConvertibleToFloat(str)
    (setq isNegative nil)
    (setq commaPosition -1)
    (setq digits (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (setq state 0)
    (cond ((= (length str) 0) (setq state 5))
	  ((char-equal (aref str 0) #\-) (setq state 1) (setq isNegative t))
	  ((member (aref str 0) digits) (setq state 2))
	  (t (setq state 5)))
    (when (/= state 5)
      (dotimes (checkedCharNr (- (length str) 1))
	(setq index (+ checkedCharNr 1))
	(case state
	      (1 (cond ((member (aref str index) digits) (setq state 2))
		       (t (setq state 5))))
	      (2 (cond ((char-equal (aref str index) #\.) (setq state 3) (setq commaPosition index))
		       ((not (member (aref str index) digits)) (setq state 5))))
	      (3 (cond ((member (aref str index) digits) (setq state 4))
		       (t (setq state 5))))
	      (4 (cond ((not (member (aref str index) digits)) (setq state 5))))
	      (5 (return)))))
    (return-from isStringConvertibleToFloat (or (= state 2) (= state 4)))) ; the string should be an integer or a float
  (setq floatResult nil)
  (when (isStringConvertibleToFloat str)
    (setq floatResult 0.0)
    (setq integerPart nil)
    (setq decimalPart nil)
    (cond ((/= commaPosition -1) ; lacking comma position means no decimal part
	   (setq decimalPart (subseq str (+ commaPosition 1)))
	   (if (not (null isNegative))
	      (setq integerPart (subseq str 1 commaPosition))
	     (setq integerPart (subseq str 0 commaPosition))))
	  ((not (null isNegative)) (setq integerPart (subseq str 1)))
	  (t (setq integerPart str)))
    (setq charToNumberHash (getDigitCharToNumberHash))
    (when (not (null decimalPart)) ; optional: get decimal part of the number
	(loop for index from 0 to (- (length decimalPart) 1)
	      do
	      (setq decimalDigitExponent (+ 1 index))
	      (setq decimalPartDigit (gethash (aref decimalPart index) charToNumberHash))
	      (setq floatResult (+ floatResult (* decimalPartDigit (expt 0.1 decimalDigitExponent))))))
    (loop for index from 0 to (- (length integerPart) 1) ; mandatory: get integer part of the number
	  do
	  (setq intDigitExponent (- (length integerPart) 1 index))
	  (setq intPartDigit (gethash (aref integerPart index) charToNumberHash))
	  (setq floatResult (+ floatResult (* intPartDigit (expt 10 intDigitExponent)))))
    (if (not (null isNegative)) ; finally add negative sign (if applicable)
	(setq floatResult (- 0 floatResult))))
  (return-from convertStringToFloat floatResult)
)
