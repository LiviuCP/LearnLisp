#| This file contains specific macros and functions that might be used by all other lib functions |#

(defmacro swapArrayItems(inputArray first second)
  "This macro is used for swapping two elements from SAME array."
  `(let ((temp (aref ,inputArray ,first)))
    (setf (aref ,inputArray ,first) (aref ,inputArray ,second))
    (setf (aref ,inputArray ,second) temp)))

(defmacro getMinMaxArrayElement(inputArray)
  "This macro is used for getting the minimum and maximum array element (integer/rational/real ... value types)."
  `(let* ((result) (currentMinimum (aref ,inputArray 0)) (currentMaximum currentMinimum))
     (dotimes (index (- (length ,inputArray) 1))
       (cond ((< (aref ,inputArray (+ index 1)) currentMinimum) (setq currentMinimum (aref ,inputArray (+ index 1))))
	     ((> (aref ,inputArray (+ index 1)) currentMaximum) (setq currentMaximum (aref ,inputArray (+ index 1))))))
     (setq result (list currentMinimum currentMaximum))))
