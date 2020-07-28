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
