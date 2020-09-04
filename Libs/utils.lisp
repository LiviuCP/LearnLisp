#| This file contains specific macros and functions that might be used by all other lib functions |#

(defmacro swap-array-items(input-array first second)
  "This macro is used for swapping two elements from SAME array."
  `(let ((temp (aref ,input-array ,first)))
    (setf (aref ,input-array ,first) (aref ,input-array ,second))
    (setf (aref ,input-array ,second) temp)))

(defmacro get-min-max-sequence-element(input-sequence)
  "This macro is used for getting the minimum and maximum array or list element (real value types)."
  `(list (reduce #'min ,input-sequence) (reduce #'max ,input-sequence)))
