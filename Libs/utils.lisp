#| This file contains specific macros and functions that might be used by all other lib functions |#

(defmacro swapArrayItems(inputArray first second)
  "This macro is used for swapping two elements from SAME array."
  `(let ((temp (aref ,inputArray ,first)))
    (setf (aref ,inputArray ,first) (aref ,inputArray ,second))
    (setf (aref ,inputArray ,second) temp)))
