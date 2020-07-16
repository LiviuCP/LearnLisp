(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")
(load "../Libs/parse.lisp")

(defun main()
  (setq bothNumbersEntered nil)
  (setq first (requestIntInputWithCondition "Enter the first number: " #'(lambda(val)(setq isDifferent (/= val 0))) "The number should be different from 0. Please try again"))
  (setq second nil)
  (when (not (null first))
    (setq second (requestIntInputWithCondition "Enter the second number: " #'(lambda(val)(setq isDifferent (/= val 0))) "The number should be different from 0. Please try again")))
  (if (not (null second))
      (progn
	(setq gcd (gCommonDiv first second))
	(format t "The greatest common divisor of ~d and ~d is: ~d~%" first second gcd)
        (when (= gcd 1)
	  (write-line "The numbers are prime among each other!"))
	(setq lcm (abs (* (floor first gcd) second))) ; make sign positive to avoid any confusion
	(format t "The lowest common multiple of ~d and ~d is: ~d~%" first second lcm))
    (write-line "You quit")))

(main)
