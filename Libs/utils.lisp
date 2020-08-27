(defmacro swapArrayItems(inputArray first second)
  `(let ((temp (aref ,inputArray ,first)))
    (setf (aref ,inputArray ,first) (aref ,inputArray ,second))
    (setf (aref ,inputArray ,second) temp)))
