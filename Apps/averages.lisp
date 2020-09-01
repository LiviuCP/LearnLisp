(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")

(defun main()
  (let ((numbers-list (list)))
    (write-line "We will calculate the arithmetic and geometric average of the absolute values of the entered numbers")
    (terpri)
    (let ((iteration-number 1))
      (loop
       (let ((number (request-integer-input (concatenate 'string "Enter number " (write-to-string iteration-number) " (press ENTER to stop): "))))
	 (cond ((not (null number)) (setq numbers-list (cons number numbers-list)) (incf iteration-number))
	       (t (return))))))
    (terpri)
    (if (= 0 (length numbers-list))
	(write-line "You haven't entered any number")
      (let ((arithmetic-average 0) (geometric-average 1))
	(loop for nr in numbers-list
	      do
	      (setq arithmetic-average (+ arithmetic-average (abs nr)))
	      (setq geometric-average (* geometric-average (abs nr))))
	(setq arithmetic-average (/ arithmetic-average (length numbers-list)))
	(setq geometric-average (sqrt (* 1.0 geometric-average)))
	(format t "You entered ~d numbers: ~a~%" (length numbers-list) (reverse numbers-list))
	(format t "The arithmetic average of the absolute values of the entered numbers is: ~d~%" arithmetic-average)
	(format t "The geometric average of the absolute values of the entered numbers is: ~f~%" geometric-average)))))

(main)
