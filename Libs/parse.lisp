(defconstant +digits+ (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) "We use this constant for mapping number characters to actual digits.")

(defun get-digit-char-to-number-hash()
  "This hash table is used for mapping digit characters to actual numeric digits when converting the string to a number."
  (let ((digits-hash (make-hash-table)))
    (setf (gethash #\0 digits-hash) 0)
    (setf (gethash #\1 digits-hash) 1)
    (setf (gethash #\2 digits-hash) 2)
    (setf (gethash #\3 digits-hash) 3)
    (setf (gethash #\4 digits-hash) 4)
    (setf (gethash #\5 digits-hash) 5)
    (setf (gethash #\6 digits-hash) 6)
    (setf (gethash #\7 digits-hash) 7)
    (setf (gethash #\8 digits-hash) 8)
    (setf (gethash #\9 digits-hash) 9)
    (return-from get-digit-char-to-number-hash digits-hash)))

(defun convert-string-to-integer(str)
  "This function is an alternative to the parse-integer function."
  (defun is-string-convertible-to-integer(str)
    (let ((is-integer t))
      (cond ((= (length str) 0) (setq is-integer nil))                                             ; case 1: no characters
	    ((and (= (length str) 1) (not (member (aref str 0) +digits+))) (setq is-integer nil))    ; case 2: single character, should be numeric
	    ((> (length str) 1)                                                                   ; case 3: more than one character, only first character can be '-' (all others should be numeric)
	     (if (not (or (member (aref str 0) +digits+) (char-equal (aref str 0) #\-)))
		 (setq is-integer nil)
	       (loop for index from 1 to (- (length str) 1)
		     do (unless (member (aref str index) +digits+)
			  (setq is-integer nil)
			  (return))))))
      (return-from is-string-convertible-to-integer is-integer)))
  (check-type str string)
  (let ((int-result))
    (when (is-string-convertible-to-integer str)
      (setq int-result 0)
      (let ((start-position 0) (char-to-number-hash (get-digit-char-to-number-hash)))
	(when (char-equal (aref str 0) #\-)
	  (setq start-position 1))
	(loop for index from start-position to (- (length str) 1)
	      do
	      (let ((integer-digit-exponent (- (length str) 1 index)) (integer-digit (gethash (aref str index) char-to-number-hash)))
		(setq int-result (+ int-result (* integer-digit (expt 10 integer-digit-exponent))))))
	(if (= start-position 1)
	    (setq int-result (- 0 int-result)))))
    (return-from convert-string-to-integer int-result)))

(defun convert-string-to-float(str)
  "This function parses a string to a float or fraction (depending on string format). It uses a state machine with following states:
  0 - Init, 1 - SignAdded, 2 - LeftDigitsAdded, 3 - CommaAdded, 4 - RightDigitsAdded, 5 - Invalid, 6 - SlashAdded, 7 - SecondSignAdded (last two states for fraction strings only)."
  (check-type str string)
  (let ((comma-position -1) (slash-position -1) (is-negative) (char-to-number-hash (get-digit-char-to-number-hash)) (left-number-string) (right-number-string) (result))
    (defun is-string-convertible-to-float(str)
      (let ((state 0) (is-division-by-zero))
	(cond ((= (length str) 0) (setq state 5))
	      ((char-equal (aref str 0) #\-) (setq state 1) (setq is-negative t))
	      ((member (aref str 0) +digits+) (setq state 2))
	      (t (setq state 5)))
	(when (/= state 5)
	  (dotimes (checked-character-number (- (length str) 1))
	    (let ((index (+ checked-character-number 1)))
	      (case state
		    (1 (cond ((member (aref str index) +digits+) (setq state 2))
			     (t (setq state 5))))
		    (2 (cond ((char-equal (aref str index) #\.) (setq state 3) (setq comma-position index))
			     ((char-equal (aref str index) #\/) (setq state 6) (setq slash-position index))
			     ((not (member (aref str index) +digits+)) (setq state 5))))
		    (3 (cond ((member (aref str index) +digits+) (setq state 4))
			     (t (setq state 5))))
		    (4 (cond ((not (member (aref str index) +digits+)) (setq state 5))))
		    (5 (return))
		    (6 (cond ((member (aref str index) +digits+) (setq state 4))
			     ((char-equal (aref str index) #\-) (setq state 7) (setq is-negative (not is-negative)))
			     (t (setq state 5))))
		    (7 (cond ((member (aref str index) +digits+) (setq state 4))
			     (t (setq state 5))))))))
        ; retrieve pure numeric substrings (left side and (optionally, if / or . separator exists) right side)
	(when (or (= state 2) (= state 4))
	  (cond ((/= comma-position -1) ; case 1: decimal string, comma available
		 (setq right-number-string (subseq str (+ comma-position 1)))
		 (if (char-equal (aref str 0) #\-)
		     (setq left-number-string (subseq str 1 comma-position))
		   (setq left-number-string (subseq str 0 comma-position))))
		((/= slash-position -1) ; case 2: fraction string, slash available
		 (if (char-equal (aref str 0) #\-)
		     (setq left-number-string (subseq str 1 slash-position))
		   (setq left-number-string (subseq str 0 slash-position)))
		 (if (char-equal (aref str (+ slash-position 1)) #\-)
		     (setq right-number-string (subseq str (+ slash-position 2)))
		   (setq right-number-string (subseq str (+ slash-position 1)))))
		((not (null is-negative)) (setq left-number-string (subseq str 1))) ; case 3a: negative integer
		(t (setq left-number-string str))) ; case 3b: positive integer
          ; if the string is a fraction it should not be convertible if a division by zero is detected
	  (when (and (= state 4) (/= slash-position -1) (= (length (string-left-trim "0" right-number-string)) 0))
	    (setq is-division-by-zero t)))
	(return-from is-string-convertible-to-float (or (= state 2) (and (= state 4) (not is-division-by-zero)))))) ; the string should be an integer or a float/fraction
    (defun get-integer-from-string(integer-string) ; converts a (checked) pure-numeric string (no negative sign) to integer, e.g. "123" to 123
      (let ((result 0))
	(loop for index from 0 to (- (length integer-string) 1)
	      do
	      (let ((digit-exponent (- (length integer-string) 1 index)) (number-digit (gethash (aref integer-string index) char-to-number-hash)))
		(setq result (+ result (* number-digit (expt 10 digit-exponent))))))
	(return-from get-integer-from-string result)))
    (defun get-decimal-from-string(decimal-string) ; convert a (checked) pure-numeric string (no negative sign) to pure decimal, e.g. "123" to 0.123
      (let ((result 0))
	(loop for index from 0 to (- (length decimal-string) 1)
	      do
	      (let ((digit-exponent (+ 1 index)) (number-digit (gethash (aref decimal-string index) char-to-number-hash)))
		(setq result (+ result (* number-digit (expt 0.1 digit-exponent))))))
	(return-from get-decimal-from-string result)))
    (when (is-string-convertible-to-float str)
      ; do actual conversion to a float value
      (setq result 0.0)
      (unless (null right-number-string)
	(if (/= comma-position -1) ; get right side number (pure decimal part / fraction denominator)
	    (setq result (get-decimal-from-string right-number-string))
	  (setq result (get-integer-from-string right-number-string))))
      (let ((integer-part (get-integer-from-string left-number-string))) ; get integer part or numerator and combine it to partial outcome to obtain the end result
	(if (/= slash-position -1)
	    (if (= 0 (rem integer-part result))
		(setq result (* 1.0 (/ integer-part result))) ; integer fraction
	      (setq result (/ integer-part result))) ; non-integer fraction, store as rational number
	  (setq result (+ result integer-part)))) ; integer part for integer/decimal string
      (unless (null is-negative) ; finally add the negative sign (if applicable)
	(setq result (- 0 result))))
    (return-from convert-string-to-float result)))
