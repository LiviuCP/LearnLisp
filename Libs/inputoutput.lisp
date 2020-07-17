(defun requestIntInput(message)
  (check-type message string)
  (setq result nil)
  (loop
   (princ message)
   (setq input (read-line))
   (setq input (string-left-trim " " input))
   (setq input (string-right-trim " " input))
   (cond ((= (length input) 0) (return))
	 ((isStringInteger input) (setq result (parse-integer input)) (return))
	 (t (write-line "You haven't entered a valid number. Please try again"))))
  (return-from requestIntInput result))

; condition should be a lambda function that contains a single param to which the converted value (result) is passed as arg, e.g. #'(lambda(val)(setq res (> val 0)))
(defun requestIntInputWithCondition(message condition conditionErrorMessage)
  (check-type message string)
  (check-type condition function)
  (check-type conditionErrorMessage string)
  (setq result nil)
  (loop
   (princ message)
   (setq input (read-line))
   (setq input (string-left-trim " " input))
   (setq input (string-right-trim " " input))
   (cond ((= (length input) 0) (return))
	 ((isStringInteger input) (setq result (parse-integer input)) (if (funcall condition result) (return) (progn (princ conditionErrorMessage) (terpri))))
	 (t (write-line "You haven't entered a valid number. Please try again"))))
  (return-from requestIntInputWithCondition result))
