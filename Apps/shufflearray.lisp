(load "../Libs/sort.lisp")
(load "../Libs/parse.lisp")

; enter array values in input file, check output in output file (modify paths if required)
(defconstant inputFile "/tmp/shufflearrayinput.txt")
(defconstant outputFile "/tmp/shufflearrayoutput.txt")

(defun main()
  ; open the files just to check they are there, create if not existing
  (open inputFile :direction :probe :if-does-not-exist :create)
  (open outputFile :direction :probe :if-does-not-exist :create)
  ; read the array elements, parse them and build the array
  (let ((elementsArray) (elementsArrayCopy) (success))
    (let ((line) (elements (list)))
      (with-open-file (inFile inputFile :direction :input)
		      (setq line (read-line inFile)))
      (if (= (length line) 0)
	  (write-line "There are no elements in the input file!")
	(progn
	  (with-input-from-string (inStr line)
				  (let ((currentElement))
				    (loop
				     (setq currentElement (read inStr nil))
				     (when (null currentElement)
				       (return))
				     (setq elements (cons (write-to-string currentElement) elements))))) ; use write-to-string to keep the element as string (otherwise convertStringToInt might throw an error)
	  (setq elementsArray (make-array (length elements)))
	  (let ((currentIndex (- (length elementsArray) 1)))
	    (setq success t)
	    (loop for element in elements
		  do
		  (let ((converted (parse-integer element :junk-allowed t)))
		    (cond ((null converted) (setq success nil) (write-line "Invalid element detected. App aborted.") (return))
			  (t (setf (aref elementsArray currentIndex) converted) (decf currentIndex 1)))))))))
    ; do the necessary calculations on the array: shuffle, counter-sort, sort etc and write results to file
    (when success
      (setq elementsArrayCopy (make-array (length elementsArray)))
      (dotimes (index (length elementsArray)) ; make an array copy in order to use two different shuffling techniques on the original array sequence (each technique is destructive)
	(setf (aref elementsArrayCopy index) (aref elementsArray index)))
      (with-open-file (outFile outputFile :direction :output)
		      (defun writeArrayAndStatistics(message outputArray statistics) ; use this function to write array shuffling statistics in a nicely formatted manner
			(format outFile message)
			(terpri outFile)
			(terpri outFile)
			(format outFile "~a~%" outputArray)
			(terpri outFile)
			(format outFile "---- ARRAY SORTING STATISTICS ----~%")
			(terpri outFile)
			(format outFile "Number of increasingly sorted sequences: ~d~%" (nth 0 statistics))
			(format outFile "Number of (strictly) decreasingly sorted sequences: ~d~%" (nth 1 statistics))
			(format outFile "Minimum number of elements in an increasingly sorted sequence: ~d~%" (nth 2 statistics))
			(format outFile "Maximum number of elements in an increasingly sorted sequence: ~d~%" (nth 3 statistics))
			(format outFile "Minimum number of elements in a (strictly) decreasingly sorted sequence: ~d~%" (nth 4 statistics))
			(format outFile "Maximum number of elements in a (strictly) decreasingly sorted sequence: ~d~%" (nth 5 statistics))
			(format outFile "Number of array elements: ~d~%" (nth 6 statistics))
			(terpri outFile))
		      (writeArrayAndStatistics "1) ORIGINAL array:" elementsArray (getSortedGroupsInfo elementsArray))
		      (shuffleArray elementsArray)
		      (writeArrayAndStatistics "2) SHUFFLED array:" elementsArray (getSortedGroupsInfo elementsArray))
		      (counterSort elementsArrayCopy)
		      (writeArrayAndStatistics "3) COUNTER-SORTED array:" elementsArrayCopy (getSortedGroupsInfo elementsArrayCopy))
		      (insertionSort elementsArray)
		      (writeArrayAndStatistics "4) SORTED array:" elementsArray (getSortedGroupsInfo elementsArray))
		      (write-line "The array has been successfully processed")
		      (format t "Check output file ~s to see the result~%" outputFile)))))

(main)
