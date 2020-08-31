(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")

(defun main()
  (let ((matrixOrder) (firstParam) (secondParam) (thirdParam) (fourthParam))
    (setq matrixOrder (request-integer-input-with-condition "Enter the requested size order for the Fibonacci matrix (press ENTER to quit): " #'(lambda(val)(let ((isGreater)) (setq isGreater (> val 0)))) "The number should be strictly positive. Please try again"))
    (unless (null matrixOrder)
      (setq firstParam (request-integer-input-with-condition "Enter the first parameter f[0][0] (press ENTER to quit): " #'(lambda(val) (let ((isGreater)) (setq isGreater (>= val 0)))) "The number should not be negative. Please try again")))
    (unless (null firstParam)
      (setq secondParam (request-integer-input-with-condition "Enter the second parameter f[0][1] (press ENTER to quit): " #'(lambda(val) (let ((isGreater)) (setq isGreater (> val firstParam)))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (unless (null secondParam)
      (setq thirdParam (request-integer-input-with-condition "Enter the third parameter f[1][0] (press ENTER to quit): " #'(lambda(val)(let ((isGreater)) (setq isGreater (> val firstParam)))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (unless (null thirdParam)
      (setq fourthParam (request-integer-input-with-condition "Enter the third parameter f[1][1] (press ENTER to quit): " #'(lambda(val)(let ((isGreater)) (setq isGreater (> val firstParam)))) (concatenate 'string "The number should be greater than " (write-to-string firstParam) ". Please try again"))))
    (if (not (null fourthParam))
	(progn
	  (terpri)
	  (princ "The resulting Fibonacci matrix is: ")
	  (terpri)
	  (print (get-fibonacci-matrix matrixOrder (list firstParam secondParam thirdParam fourthParam))))
      (write-line "You quit"))))

(main)
