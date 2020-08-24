; performs a simple "shuffle" of the array by ordering the elements: a[0] <= a[1] >= a[2] <= a[3] ... or a[0] >= a[1] <= a[2] >= a[3] ...
; sortKey is the ordering criteria (e.g. a<=b), reverseKey is the ordering criteria of first 2 elements (according (nil) or reversed (t) to sortKey); criteria is that continually reversed for each step
(defun counterSort(inputArray &optional reverseKey sortKey) 
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (let ((keyOrderRequired (not reverseKey)))
    (dotimes (index (- (length inputArray) 1))
      (when (or (and keyOrderRequired (not (funcall sortKey (aref inputArray index) (aref inputArray (+ index 1))))) (and (not keyOrderRequired) (funcall sortKey (aref inputArray index) (aref inputArray (+ index 1)))))
	  (let ((temp (aref inputArray index)))
	    (setf (aref inputArray index) (aref inputArray (+ index 1)))
	    (setf (aref inputArray (+ index 1)) temp)))
      (setq keyOrderRequired (not keyOrderRequired))))) ; change ordering type for each step (according to key (e.g. increasing) / reverse to key (e.g. decreasing))

; performs more complex mixing (shuffling) of array elements by swapping elements from right half of the array with randomly chosen elements from left half
(defun shuffleArray(inputArray)
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (when (> (length inputArray) 2)
    (let ((leftIntervalEnd (floor (length inputArray) 2)) (rightIntervalBegin) (vacancies) (duplicates (list)))
      ; divide array into two halves, elements from second half to be walked through and swapped with randomly chosen elements from first half (middle element might remain in a fixed position)
      (setq rightIntervalBegin (+ leftIntervalEnd (rem (length inputArray) 2)))
      (setq vacancies (make-array `(,leftIntervalEnd)))
      (loop for index from rightIntervalBegin to (- (length inputArray) 1)
	    do (let ((randomIndex (random leftIntervalEnd)))
		 (cond ((null (aref vacancies randomIndex))
			(setf (aref vacancies randomIndex) t)
			(let ((temp (aref inputArray index)))
			  (setf (aref inputArray index) (aref inputArray randomIndex))
			  (setf (aref inputArray randomIndex) temp)))
		       (t
			(setq duplicates (cons index duplicates)))))) ; if randomly chosen element from left had already been swapped: put the element from the right side into a separate list (reverse order)
      (let ((currentVacancyIndex 0))
	; fill in the vacant positions from left side with elements from the duplicates list
	(dolist (element duplicates)
	  (loop
	   (when (null (aref vacancies currentVacancyIndex))
	     (let ((temp (aref inputArray currentVacancyIndex)))
		   (setf (aref inputArray currentVacancyIndex) (aref inputArray element))
		   (setf (aref inputArray element) temp))
	     (setf (aref vacancies currentVacancyIndex) t) ; not really needed by "better safe than sorry"
	     (incf currentVacancyIndex 1)
	     (return))
	   (incf currentVacancyIndex 1))))
      ; finally swap elements pairwise to ensure a better mix (controversial, to be checked further)
      (do ((index 0 (incf index 2)))
	  ((>= index (- (length inputArray) 1)))
	  (let ((temp (aref inputArray index)))
		(setf (aref inputArray index) (aref inputArray (+ index 1)))
		(setf (aref inputArray (+ index 1)) temp))))))

(defun bubbleSort(inputArray &key sortKey left right)
  (check-type inputArray array)
  (when (not (null left))
    (check-type left integer)
    (assert (and (>= left 0) (< left (length inputArray))) (left) "Left interval index out of range"))
  (when (not (null right))
    (check-type right integer)
    (assert (and (> right 0) (<= right (length inputArray))) (right) "Right interval index out of range"))
  (when (and (not (null left)) (not (null right)))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (>= (length inputArray) 2)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length inputArray)))
    (loop
     (let ((sortingPerformed))
       (loop for index from left to (- right 2)
	     do
	     (when (not (funcall sortKey (aref inputArray index) (aref inputArray (+ index 1))))
	       (let ((temp (aref inputArray index)))
		 (setf (aref inputArray index) (aref inputArray (+ index 1)))
		 (setf (aref inputArray (+ index 1)) temp)
		 (setq sortingPerformed t))))
       (when (not sortingPerformed) ; stop when no item swap performed along the iteration
	 (return))))))

(defun insertionSort(inputArray &optional sortKey)
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length inputArray) 0)
    (loop for index from 1 to (- (length inputArray) 1)
	  do
	  (let ((elementToInsert index) (checkedIndex (- index 1)))
	    (loop
	     (when (< checkedIndex 0)
	       (return))
	     (if (not (funcall sortKey (aref inputArray checkedIndex) (aref inputArray elementToInsert)))
		 (progn
		   (let ((temp (aref inputArray checkedIndex)))
		     (setf (aref inputArray checkedIndex) (aref inputArray elementToInsert))
		     (setf (aref inputArray elementToInsert) temp))
		   (setq elementToInsert checkedIndex))
	       (return))
	     (decf checkedIndex 1))))))

(defun mergeSort(inputArray &optional sortKey)
  (defun doMergeSort(inputArray auxArray startIndex endIndex sortKey)
    (when (/= startIndex endIndex)
      (let ((midIndex (floor (+ startIndex endIndex) 2)) (firstIndex) (secondIndex) (writeIndex))
	(doMergeSort inputArray auxArray startIndex midIndex sortKey)
	(doMergeSort inputArray auxArray (+ midIndex 1) endIndex sortKey)
	(setq firstIndex startIndex)
	(setq secondIndex  (+ midIndex 1))
	(setq writeIndex startIndex)
	(loop
	 (if (or (> firstIndex midIndex) (> secondIndex endIndex))
	     (return))
	 (cond ((not (funcall sortKey (aref inputArray secondIndex) (aref inputArray firstIndex))) (setf (aref auxArray writeIndex) (aref inputArray firstIndex)) (incf firstIndex 1))
	       (t (setf (aref auxArray writeIndex) (aref inputArray secondIndex)) (incf secondIndex 1)))
	 (incf writeIndex 1))
	(if (<= firstIndex midIndex)
	    (loop
	     (when (> writeIndex endIndex)
		 (return))
	     (setf (aref auxArray writeIndex) (aref inputArray firstIndex))
	     (incf firstIndex 1)
	     (incf writeIndex 1))
	  (if (<= secondIndex endIndex)
	      (loop
	       (when (> writeIndex endIndex)
		   (return))
	       (setf (aref auxArray writeIndex) (aref inputArray secondIndex))
	       (incf secondIndex 1)
	       (incf writeIndex 1))))
	(loop for firstIndex from startIndex to endIndex
	      do (setf (aref inputArray firstIndex) (aref auxArray firstIndex))))))
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (let ((nrOfElements (length inputArray)))
    (when (> (length inputArray) 0)
      (let ((auxArray (make-array `(,(length inputArray)))))
	(dotimes (index nrOfElements)
	  (setf (aref auxArray index) (aref inputArray index)))
	(doMergeSort inputArray auxArray 0 (- nrOfElements 1) sortKey)))))

(defun quickSort (inputArray &optional sortkey)
  (defun doQuickSort(inputArray beginIndex endIndex sortKey)
    (when (/= beginIndex endIndex)
      (if (= beginIndex (- endIndex 1))
	  (when (not (funcall sortKey (aref inputArray beginIndex) (aref inputArray endIndex)))
	    (let ((temp (aref inputArray beginIndex)))
	      (setf (aref inputArray beginIndex) (aref inputArray endIndex))
	      (setf (aref inputArray endIndex) temp)))
	(progn
	  (let ((pivot (aref inputArray beginIndex)) (leftIndex (+ beginIndex 1)) (rightIndex endIndex))
	    (loop
	     (when (> leftIndex rightIndex)
	       (return))
	     (loop
	      (when (not (and (or (not (funcall sortKey pivot (aref inputArray leftIndex))) (= pivot (aref inputArray leftIndex))) (< leftIndex endIndex)))
		(return))
	      (incf leftIndex 1))
	     (loop
	      (when (not (and (not (funcall sortKey (aref inputArray rightIndex) pivot)) (> rightIndex beginIndex)))
		(return))
	      (decf rightIndex 1))
	     (if (< leftIndex rightIndex)
		 (let ((temp (aref inputArray leftIndex)))
		   (setf (aref inputArray leftIndex) (aref inputArray rightIndex))
		   (setf (aref inputArray rightIndex) temp))
	       (return)))
	    (when (> rightIndex beginIndex)
	      (let ((temp (aref inputArray beginIndex)))
		(setf (aref inputArray beginIndex) (aref inputArray rightIndex))
		(setf (aref inputArray rightIndex) temp))
	      (doQuickSort inputArray beginIndex (- rightIndex 1) sortKey))
	    (when (< rightIndex endIndex)
	      (doQuickSort inputArray (+ rightIndex 1) endIndex sortKey)))))))
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (counterSort inputArray) ; ensure the array elements are shuffled to avoid having sorted sequences prior to passing it to quick sort
  (doQuickSort inputArray 0 (- (length inputArray) 1) sortKey))

(defun heapSort (inputArray &optional sortKey)
  (defun buildHeap (inputArray sortKey)
    (dotimes (index (length inputArray))
      (let ((checkedElementIndex index))
	(loop
	 (when (<= checkedElementIndex 0)
	   (return))
	 (let ((parentElementIndex (floor (- checkedElementIndex 1) 2)))
	   (if (not (funcall sortKey (aref inputArray checkedElementIndex) (aref inputArray parentElementIndex)))
	       (progn
		 (let ((temp (aref inputArray parentElementIndex)))
		   (setf (aref inputArray parentElementIndex) (aref inputArray checkedElementIndex))
		   (setf (aref inputArray checkedElementIndex) temp))
		 (setq checkedElementIndex parentElementIndex))
	     (return)))))))
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length inputArray) 0)
    (buildHeap inputArray sortKey)
    (let ((currentToSortIndex (- (length inputArray) 1)))
      (loop
       (when (<= currentToSortIndex 0)
	 (return))
       (let ((temp (aref inputArray 0)))
	 (setf (aref inputArray 0) (aref inputArray currentToSortIndex))
	 (setf (aref inputArray currentToSortIndex) temp))
       (decf currentToSortIndex 1)
       (let ((checkedElementIndex 0))
	 (loop
	  (when (<= currentToSortIndex checkedElementIndex)
	    (return))
	  (let ((leftChildIndex (+ (* checkedElementIndex 2) 1)))
	    (cond ((> currentToSortIndex leftChildIndex)
		   (let ((rightChildIndex (+ leftChildIndex 1)) (toSwapIndex leftChildIndex))
		     (when (not (funcall sortKey (aref inputArray rightChildIndex) (aref inputArray leftChildIndex)))
		       (setq toSwapIndex rightChildIndex))
		     (cond ((not (funcall sortKey (aref inputArray toSwapIndex) (aref inputArray checkedElementIndex)))
			    (let ((temp (aref inputArray checkedElementIndex)))
			      (setf (aref inputArray checkedElementIndex) (aref inputArray toSwapIndex))
			      (setf (aref inputArray toSwapIndex) temp))
			    (setq checkedElementIndex toSwapIndex))
			   (t (return)))))
		  ((= leftChildIndex currentToSortIndex)
		   (when (not (funcall sortKey (aref inputArray leftChildIndex) (aref inputArray checkedElementIndex)))
		     (let ((temp (aref inputArray checkedElementIndex)))
		       (setf (aref inputArray checkedElementIndex) (aref inputArray leftChildIndex))
		       (setf (aref inputArray leftChildIndex) temp)))
		     (return))
		  (t (return))))))))))

(defun getSortedGroupsInfo(inputArray &optional sortKey)
  (defun getMinNrOfSortedElements(currentMinNr providedMinNr)
    (let ((newMinNr))
      (if (= currentMinNr 0)
	  (setq newMinNr providedMinNr)
	(setq newMinNr (min currentMinNr providedMinNr)))))
  (check-type inputArray array)
  (dotimes (index (length inputArray))
    (check-type (aref inputArray index) (or integer float rational)))
  (if (not (null sortKey))
      (check-type sortKey function)
    (setq sortKey (lambda(a b)(let ((result))(setq result (<= a b))))))
  ; states: 0 - initial, 1 - sorted by key (e.g a>=b), 2 - sorted against key (e.g. a<b)
  (let ((state 0) (currentNrOfGroupElements 0) (groupsSortedByKey 0) (groupsSortedAgainstKey 0) (minNrOfElemSortedByKey 0) (maxNrOfElemSortedByKey 0) (minNrOfElemSortedAgainstKey 0) (maxNrOfElemSortedAgainstKey 0))
    ; a sorted group should have minimum three continguously sorted elements
    (when (>= (length inputArray) 3)
      (dotimes (index (- (length inputArray) 1))
	(case state
	      (0 (setq currentNrOfGroupElements 2)
		 (cond ((funcall sortKey (aref inputArray index) (aref inputArray (+ index 1)))
			(setq state 1))
		       (t (setq state 2))))
	      (1 (cond ((funcall sortKey (aref inputArray index) (aref inputArray (+ index 1)))
			(incf currentNrOfGroupElements 1))
		       (t (when (>= currentNrOfGroupElements 3)
			    (incf groupsSortedByKey 1)
			    (setq minNrOfElemSortedByKey (getMinNrOfSortedElements minNrOfElemSortedByKey currentNrOfGroupElements))
			    (setq maxNrOfElemSortedByKey (max maxNrOfElemSortedByKey currentNrOfGroupElements)))
			  (setq currentNrOfGroupElements 2)
			  (setq state 2))))
	      (2 (cond ((funcall sortKey (aref inputArray index) (aref inputArray (+ index 1)))
			(when (>= currentNrOfGroupElements 3)
			  (incf groupsSortedAgainstKey 1)
			  (setq minNrOfElemSortedAgainstKey (getMinNrOfSortedElements minNrOfElemSortedAgainstKey currentNrOfGroupElements))
			  (setq maxNrOfElemSortedAgainstKey (max maxNrOfElemSortedAgainstKey currentNrOfGroupElements)))
			(setq currentNrOfGroupElements 2)
			(setq state 1))
		       (t (incf currentNrOfGroupElements 1))))))
      ; at the end of the sequence the state might not change anymore, however the last elements might be a sorted group not taken into account (corner case)
      (when (>= currentNrOfGroupElements 3)
	(cond ((= state 1)
	       (incf groupsSortedByKey 1)
	       (setq minNrOfElemSortedByKey (getMinNrOfSortedElements minNrOfElemSortedByKey currentNrOfGroupElements))
	       (setq maxNrOfElemSortedByKey (max maxNrOfElemSortedByKey currentNrOfGroupElements)))
	      (t
	       (incf groupsSortedAgainstKey 1)
	       (setq minNrOfElemSortedAgainstKey (getMinNrOfSortedElements minNrOfElemSortedAgainstKey currentNrOfGroupElements))
	       (setq maxNrOfElemSortedAgainstKey(max maxNrOfElemSortedAgainstKey currentNrOfGroupElements))))))
    (return-from getSortedGroupsInfo (list groupsSortedByKey groupsSortedAgainstKey minNrOfElemSortedByKey maxNrOfElemSortedByKey minNrOfElemSortedAgainstKey maxNrOfElemSortedAgainstKey (length inputArray)))))
