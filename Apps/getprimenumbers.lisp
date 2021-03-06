(load "../Libs/mathfunctions.lisp")
(load "../Libs/inputoutput.lisp")

(defconstant +output-file+ "/tmp/primes.txt")

(defun main()
  (let ((left) (right))
    (setq left (request-integer-input-with-condition "Enter the left interval margin: " #'(lambda(val)(let ((is-greater)) (setq is-greater (> val 1)))) "The number should be greater than 1. Please try again"))
    (unless (null left)
      (setq right (request-integer-input-with-condition "Enter the right interval margin: " #'(lambda(val)(let ((is-greater)) (setq is-greater (> val 1)))) "The number should be greater than 1. Please try again")))
    (cond ((null right) (write-line "You quit"))
	  (t (let ((primes (list)))
	       (cond ((> left right) (let ((temp left)) (setq left right) (setq right temp))))
	       (terpri)
	       (princ "Retrieving prime numbers from interval: ")
	       (cond ((< left right) (format t "[~d; ~d]~%~%" left right) (setq primes (get-prime-numbers right left)))
		     (t (format t "[~d; ~d]~%~%" +first-relevant-prime+ right) (setq primes (get-prime-numbers left)))) ; //equal margins, only one threshold, display prime numbers starting with 2
	       (princ "Done!")
	       (terpri)
	       (terpri)
	       (if (= (length primes) 0)
		   (write-line "There are no prime numbers within this interval!")
		 (progn
		   (with-open-file (stream +output-file+ :direction :output)
				   (format stream "Found following prime numbers in interval [~d; ~d]: ~%~%" left right)
				   (dotimes (index (length primes))
				     (format stream "Position: ~d~t~t~t~tValue: ~d~%" (+ index 1) (aref primes index)))
				   (format t "~d numbers found~%~%" (length primes))
				   (format t "Please check output file ~d~%" +output-file+)))))))))

(main)
