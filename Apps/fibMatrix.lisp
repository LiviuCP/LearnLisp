(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")
(load "../Libs/parse.lisp")

(defun main()
  (let ((matrixOrder) (firstParam) (secondParam) (thirdParam) (fourthParam))
    (setq matrixOrder (requestIntInputWithCondition "Enter the requested size order for the Fibonacci matrix (press ENTER to quit): " #'(lambda(val)(setq isGreater (> val 0))) "The number should be strictly positive. Please try again"))
    (when (not (null matrixOrder))
      (setq firstParam (requestIntInputWithCondition "Enter the first parameter f[0][0] (press ENTER to quit): " #'(lambda(val)(setq isGreater (>= val 0))) "The number should be strictly positive. Please try again")))
    (when (not (null firstParam))
      (setq secondParam (requestIntInputWithCondition "Enter the second parameter f[0][1] (press ENTER to quit): " #'(lambda(val)(setq isGreater (> val firstParam))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (when (not (null secondParam))
	(setq thirdParam (requestIntInputWithCondition "Enter the third parameter f[1][0] (press ENTER to quit): " #'(lambda(val)(setq isGreater (> val firstParam))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (when (not (null thirdParam))
      (setq fourthParam (requestIntInputWithCondition "Enter the third parameter f[1][1] (press ENTER to quit): " #'(lambda(val)(setq isGreater (> val firstParam))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (if (not (null fourthParam))
	(progn
	  (terpri)
	  (princ "The resulting Fibonacci matrix is: ")
	  (terpri)
	  (print (getFibonacciMatrix matrixOrder (list firstParam secondParam thirdParam fourthParam))))
      (write-line "You quit"))))

(main)