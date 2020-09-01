(load (merge-pathnames "parse.lisp" *load-truename*))

(defun request-integer-input(message)
  "This function is used for taking over pure integer user input. It will exit only if the user aborts data entry or a correct integer has been entered as string."
  (check-type message string)
  (let ((result))
    (loop
     (princ message)
     (let ((input (read-line)))
	   (setq input (string-left-trim " " input))
	   (setq input (string-right-trim " " input))
	   (cond ((= (length input) 0) (return))
		 (t (setq result (convert-string-to-integer input))
		    (cond ((not (null result)) (return))
			  (t (write-line "You haven't entered a valid number. Please try again")))))))
    (return-from request-integer-input result)))

; condition should be a lambda function that contains a single param to which the converted value (result) is passed as arg, e.g. #'(lambda(val)(setq res (> val 0)))
(defun request-integer-input-with-condition(message condition condition-error-message)
  "This function is used for taking over integer user input that satisfies a specific condition. Exit criteria is user abort or entering an integer that satisfies the provided condition."
  (check-type message string)
  (check-type condition function)
  (check-type condition-error-message string)
  (let ((result))
    (loop
     (princ message)
     (let ((input (read-line)))
       (setq input (string-left-trim " " input))
       (setq input (string-right-trim " " input))
       (cond ((= (length input) 0) (return))
	     (t (let ((temp-result (convert-string-to-integer input)))
		  (cond ((not (null temp-result))
			 (if (funcall condition temp-result)
			     (progn
			       (setq result temp-result)
			       (return))
			   (progn
			     (princ condition-error-message)
			     (terpri))))
			(t (write-line "You haven't entered a valid number. Please try again"))))))))
    (return-from request-integer-input-with-condition result)))
