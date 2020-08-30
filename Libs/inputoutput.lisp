(load (merge-pathnames "parse.lisp" *load-truename*))

(defun requestIntInput(message)
  "This function is used for taking over pure integer user input. It will exit only if the user aborts data entry or a correct integer has been entered as string."
  (check-type message string)
  (let ((result))
    (loop
     (princ message)
     (let ((input (read-line)))
	   (setq input (string-left-trim " " input))
	   (setq input (string-right-trim " " input))
	   (cond ((= (length input) 0) (return))
		 (t (setq result (convertStringToInt input))
		    (cond ((not (null result)) (return))
			  (t (write-line "You haven't entered a valid number. Please try again")))))))
    (return-from requestIntInput result)))

; condition should be a lambda function that contains a single param to which the converted value (result) is passed as arg, e.g. #'(lambda(val)(setq res (> val 0)))
(defun requestIntInputWithCondition(message condition conditionErrorMessage)
  "This function is used for taking over integer user input that satisfies a specific condition. Exit criteria is user abort or entering an integer that satisfies the provided condition."
  (check-type message string)
  (check-type condition function)
  (check-type conditionErrorMessage string)
  (let ((result))
    (loop
     (princ message)
     (let ((input (read-line)))
       (setq input (string-left-trim " " input))
       (setq input (string-right-trim " " input))
       (cond ((= (length input) 0) (return))
	     (t (let ((tempResult (convertStringToInt input)))
		  (cond ((not (null tempResult))
			 (if (funcall condition tempResult)
			     (progn
			       (setq result tempResult)
			       (return))
			   (progn
			     (princ conditionErrorMessage)
			     (terpri))))
			(t (write-line "You haven't entered a valid number. Please try again"))))))))
    (return-from requestIntInputWithCondition result)))
