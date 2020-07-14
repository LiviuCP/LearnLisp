(load "../Libs/parse.lisp")
(load "../Libs/mathfunctions.lisp")

(defun requestInput(message)
  (check-type message string)
  (setq result nil)
  (loop
   (princ message)
   (setq input (read-line))
   (setq input (string-left-trim " " input))
   (setq input (string-right-trim " " input))
   (cond ((= (length input) 0) (return))
	 ((isStringInteger input) (setq result (parse-integer input)) (if (/= result 0) (return) (write-line "The number should be different from 0. Please try again")))
	 (t (write-line "Invalid number. It should be an integer. Please try again"))))
   (return-from requestInput result))

(defun main()
  (setq bothNumbersEntered nil)
  (setq first (requestInput "Enter the first number: "))
  (setq second nil)
  (when (not (null first))
    (setq second (requestInput "Enter the second number: ")))
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
