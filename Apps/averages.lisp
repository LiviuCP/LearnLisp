(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")
(load "../Libs/parse.lisp")

(defun main()
  (setq numbersList (list))
  (setq iterationNr 1)
  (write-line "We will calculate the arithmetic and geometric average of the absolute values of the entered numbers")
  (terpri)
  (loop
   (setq number (requestIntInput (concatenate 'string "Enter number " (write-to-string iterationNr) " (press ENTER to stop): ")))
   (cond ((not (null number)) (setq numbersList (cons number numbersList)) (incf iterationNr))
	 (t (return))))
  (setq arithAvg 0)
  (setq geomAvg 1)
  (terpri)
  (if (= 0 (length numbersList))
      (write-line "You haven't entered any number")
    (progn
      (loop for nr in numbersList
	    do
	      (setq arithAvg (+ arithAvg (abs nr)))
	      (setq geomAvg (* geomAvg (abs nr))))
      (setq arithAvg (/ arithAvg (length numbersList)))
      (setq geomAvg (sqrt (* 1.0 geomAvg)))
      (format t "You entered ~d numbers: ~a~%" (length numbersList) (reverse numbersList))
      (format t "The arithmetic average of the absolute values of the entered numbers is: ~d~%" arithAvg)
      (format t "The geometric average of the absolute values of the entered numbers is: ~f~%" geomAvg))))

(main)
