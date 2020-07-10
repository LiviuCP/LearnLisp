 ; calculates the greatest common divisor by remainder method
(defun gCommonDiv (first second)
  (check-type first integer)
  (check-type second integer)
  (assert (and (/= first 0) (/= second 0)) (first second) "Both arguments should be different from 0")
  (setq result 1)
  (setq divided first)
  (setq divider second)
  (loop
   (setq remainder (rem divided divider))
   (cond ((= remainder 0)(setq result divider)(return))
	 (t (setq divided divider) (setq divider remainder))))
  (return-from gCommonDiv result))
