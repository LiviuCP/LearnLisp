(load "../Libs/sort.lisp")
(load "../Libs/parse.lisp")

; enter array values in input file, check output in output file (modify paths if required)
(defconstant +input-file+ "/tmp/shufflearrayinput.txt")
(defconstant +output-file+ "/tmp/shufflearrayoutput.txt")

(defun main()
  ; open the files just to check they are there, create if not existing
  (open +input-file+ :direction :probe :if-does-not-exist :create)
  (open +output-file+ :direction :probe :if-does-not-exist :create)
  ; read the array elements, parse them and build the array
  (let ((elements-array) (elements-array-copy) (success))
    (let ((line) (elements (list)))
      (with-open-file (input-file-stream +input-file+ :direction :input)
		      (setq line (read-line input-file-stream)))
      (if (= (length line) 0)
	  (write-line "There are no elements in the input file!")
	(progn
	  (with-input-from-string (input-string-stream line)
				  (let ((current-element))
				    (loop
				     (setq current-element (read input-string-stream nil))
				     (when (null current-element)
				       (return))
				     (setq elements (cons (write-to-string current-element) elements))))) ; use write-to-string to keep the element as string (otherwise convert-string-to-integer might throw an error)
	  (setq elements-array (make-array (length elements)))
	  (let ((current-index (- (length elements-array) 1)))
	    (setq success t)
	    (loop for element in elements
		  do
		  (let ((converted (convert-string-to-integer element))) ; option 1: integer
		    (when (null converted)
		      (setq converted (convert-string-to-float element)) ; option 2: decimal (float or fraction; resulting integers, e.g. string "4/2" to be converted to float)
		      (when (null converted)
			(setq success nil)
			(write-line "Invalid element detected. App aborted.")
			(return)))
		    (when success
		      (setf (aref elements-array current-index) converted)
		      (decf current-index 1))))))))
    ; do the necessary calculations on the array: shuffle, counter-sort, sort etc and write results to file
    (when success
      (setq elements-array-copy (make-array (length elements-array)))
      (dotimes (index (length elements-array)) ; make an array copy in order to use two different shuffling techniques on the original array sequence (each technique is destructive)
	(setf (aref elements-array-copy index) (aref elements-array index)))
      (with-open-file (output-file-stream +output-file+ :direction :output)
		      (defun write-array-and-statistics(message output-array statistics) ; use this function to write array shuffling statistics in a nicely formatted manner
			(format output-file-stream message)
			(terpri output-file-stream)
			(terpri output-file-stream)
			(format output-file-stream "~a~%" output-array)
			(terpri output-file-stream)
			(format output-file-stream "---- ARRAY SORTING STATISTICS ----~%")
			(terpri output-file-stream)
			(format output-file-stream "Number of increasingly sorted sequences: ~d~%" (nth 0 statistics))
			(format output-file-stream "Number of (strictly) decreasingly sorted sequences: ~d~%" (nth 1 statistics))
			(format output-file-stream "Minimum number of elements in an increasingly sorted sequence: ~d~%" (nth 2 statistics))
			(format output-file-stream "Maximum number of elements in an increasingly sorted sequence: ~d~%" (nth 3 statistics))
			(format output-file-stream "Minimum number of elements in a (strictly) decreasingly sorted sequence: ~d~%" (nth 4 statistics))
			(format output-file-stream "Maximum number of elements in a (strictly) decreasingly sorted sequence: ~d~%" (nth 5 statistics))
			(format output-file-stream "Number of array elements: ~d~%" (nth 6 statistics))
			(terpri output-file-stream))
		      (write-array-and-statistics "1) ORIGINAL array:" elements-array (get-sorted-groups-info elements-array))
		      (shuffle-array elements-array)
		      (write-array-and-statistics "2) SHUFFLED array:" elements-array (get-sorted-groups-info elements-array))
		      (counter-sort elements-array-copy)
		      (write-array-and-statistics "3) COUNTER-SORTED array:" elements-array-copy (get-sorted-groups-info elements-array-copy))
		      (insertion-sort elements-array)
		      (write-array-and-statistics "4) SORTED array:" elements-array (get-sorted-groups-info elements-array))
		      (write-line "The array has been successfully processed")
		      (format t "Check output file ~s to see the result~%" +output-file+)))))

(main)
