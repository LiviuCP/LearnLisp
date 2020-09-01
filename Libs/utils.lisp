#| This file contains specific macros and functions that might be used by all other lib functions |#

(defmacro swap-array-items(input-array first second)
  "This macro is used for swapping two elements from SAME array."
  `(let ((temp (aref ,input-array ,first)))
    (setf (aref ,input-array ,first) (aref ,input-array ,second))
    (setf (aref ,input-array ,second) temp)))

(defmacro get-min-max-array-element(input-array)
  "This macro is used for getting the minimum and maximum array element (integer/rational/real ... value types)."
  `(let* ((result) (currentMinimum (aref ,input-array 0)) (currentMaximum currentMinimum))
     (dotimes (index (- (length ,input-array) 1))
       (cond ((< (aref ,input-array (+ index 1)) currentMinimum) (setq currentMinimum (aref ,input-array (+ index 1))))
	     ((> (aref ,input-array (+ index 1)) currentMaximum) (setq currentMaximum (aref ,input-array (+ index 1))))))
     (setq result (list currentMinimum currentMaximum))))
