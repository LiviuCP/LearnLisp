#| This file contains sorting algorithm implementations and other shuffle and statistics functions.

All sorting algorithms have the possibility to do sub-sequence sorting in arrays, e.g. sort elements with indexes 2 through 5 from a 10 elements array. |#

(load (merge-pathnames "utils.lisp" *load-truename*))

;;; sortKey is the ordering criteria (e.g. a<=b), reverse-key is the ordering criteria of first 2 elements (according (nil) or reversed (t) to sortKey); criteria is that continually reversed for each step
(defun counter-sort(input-array &optional reverse-key sort-key) 
  "This function performs a simple \"shuffle\" of the array by ordering the elements: a[0] <= a[1] >= a[2] <= a[3] ... or a[0] >= a[1] <= a[2] >= a[3] ..."
  (check-type input-array array)
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (let ((key-order-required (not reverse-key)))
    (dotimes (index (- (length input-array) 1))
      (when (or (and key-order-required (not (funcall sort-key (aref input-array index) (aref input-array (+ index 1))))) (and (not key-order-required) (funcall sort-key (aref input-array index) (aref input-array (+ index 1)))))
	(swap-array-items input-array index (+ index 1)))
      (setq key-order-required (not key-order-required))))) ; change ordering type for each step (according to key (e.g. increasing) / reverse to key (e.g. decreasing))

(defun shuffle-array(input-array)
  "This function performs more complex mixing (shuffling) of array elements by swapping elements from right half of the array with randomly chosen elements from left half."
  (check-type input-array array)
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (when (> (length input-array) 2)
    (let* ((left-interval-end (floor (length input-array) 2)) (right-interval-begin (+ left-interval-end (rem (length input-array) 2))) (vacancies (make-array `(,left-interval-end))) (duplicates (list)))
      ; divide array into two halves, elements from second half to be walked through and swapped with randomly chosen elements from first half (middle element might remain in a fixed position)
      (loop for index from right-interval-begin to (- (length input-array) 1)
	    do (let ((random-index (random left-interval-end)))
		 (cond ((null (aref vacancies random-index))
			(setf (aref vacancies random-index) t)
			(swap-array-items input-array index random-index))
		       (t
			(setq duplicates (cons index duplicates)))))) ; if randomly chosen element from left had already been swapped: put the element from the right side into a separate list (reverse order)
      (let ((current-vacancy-index 0))
	; fill in the vacant positions from left side with elements from the duplicates list
	(dolist (element duplicates)
	  (loop
	   (when (null (aref vacancies current-vacancy-index))
	     (swap-array-items input-array current-vacancy-index element)
	     (setf (aref vacancies current-vacancy-index) t) ; not really needed by "better safe than sorry"
	     (incf current-vacancy-index 1)
	     (return))
	   (incf current-vacancy-index 1))))
      ; finally swap elements pairwise to ensure a better mix (controversial, to be checked further)
      (do ((index 0 (incf index 2)))
	  ((>= index (- (length input-array) 1)))
	  (swap-array-items input-array index (+ index 1))))))

(defun bubble-sort(input-array &key sort-key left right)
  "This function implements the simplest (brute-force) algorithm: bubble sort."
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (>= (length input-array) 2)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (when (> (- right left) 1) ; there should be minimum 2 elements to be sorted
      (loop
       (let ((sorting-performed))
	 (loop for index from left to (- right 2)
	       do
	       (unless (funcall sort-key (aref input-array index) (aref input-array (+ index 1)))
		 (swap-array-items input-array index (+ index 1))
		 (setq sorting-performed t)))
	 (unless sorting-performed ; stop when no item swap performed along the iteration
	   (return)))))))

(defun insertion-sort(input-array &key sort-key left right)
  "This function implements the insertion sort algorithm."
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length input-array) 1)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (let ((sorting-length (- right left)))
      (when (> sorting-length 1)
	(let ((sequence-to-sort (make-array `(,sorting-length) :displaced-to input-array :displaced-index-offset left)))
	  (loop for index from 1 to (- sorting-length 1)
		do
		(let ((element-to-insert index) (checked-index (- index 1)))
		  (loop
		   (when (< checked-index 0)
		     (return))
		   (cond ((not (funcall sort-key (aref sequence-to-sort checked-index) (aref sequence-to-sort element-to-insert)))
			  (swap-array-items sequence-to-sort checked-index element-to-insert)
			  (setq element-to-insert checked-index))
			 (t (return)))
		   (decf checked-index 1)))))))))

(defun merge-sort(input-array &key sort-key left right)
  "This function implements the merge sort algorithm."
  (defun do-merge-sort(input-array auxiliar-array start-index end-index sort-key)
    (when (/= start-index end-index)
      (let* ((mid-index (floor (+ start-index end-index) 2)) (first-index start-index) (second-index (+ mid-index 1)) (write-index start-index))
	(do-merge-sort input-array auxiliar-array start-index mid-index sort-key)
	(do-merge-sort input-array auxiliar-array (+ mid-index 1) end-index sort-key)
	(loop
	 (if (or (> first-index mid-index) (> second-index end-index))
	     (return))
	 (cond ((not (funcall sort-key (aref input-array second-index) (aref input-array first-index))) (setf (aref auxiliar-array write-index) (aref input-array first-index)) (incf first-index 1))
	       (t (setf (aref auxiliar-array write-index) (aref input-array second-index)) (incf second-index 1)))
	 (incf write-index 1))
	(if (<= first-index mid-index)
	    (loop
	     (when (> write-index end-index)
		 (return))
	     (setf (aref auxiliar-array write-index) (aref input-array first-index))
	     (incf first-index 1)
	     (incf write-index 1))
	  (if (<= second-index end-index)
	      (loop
	       (when (> write-index end-index)
		   (return))
	       (setf (aref auxiliar-array write-index) (aref input-array second-index))
	       (incf second-index 1)
	       (incf write-index 1))))
	(loop for first-index from start-index to end-index
	      do (setf (aref input-array first-index) (aref auxiliar-array first-index))))))
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length input-array) 1)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (let ((sorting-length (- right left)))
      (when (> sorting-length 1)
	(let* ((sequence-to-sort (make-array `(,sorting-length) :displaced-to input-array :displaced-index-offset left)) (auxiliar-array (make-array `(,sorting-length))))
	  (dotimes (index sorting-length)
	    (setf (aref auxiliar-array index) (aref sequence-to-sort index)))
	  (do-merge-sort sequence-to-sort auxiliar-array 0 (- sorting-length 1) sort-key))))))

(defun quick-sort (input-array &key sort-key left right)
  "This function implements the quick sort algorithm. The elements are \"shuffled\" prior to sorting so quadratic time scenarios are avoided as much as possible."
  (defun do-quick-sort(input-array begin-index end-index sort-key)
    (when (/= begin-index end-index)
      (if (= begin-index (- end-index 1))
	  (unless (funcall sort-key (aref input-array begin-index) (aref input-array end-index))
	    (swap-array-items input-array begin-index end-index))
	(progn
	  (let ((pivot (aref input-array begin-index)) (left-index (+ begin-index 1)) (right-index end-index))
	    (loop
	     (when (> left-index right-index)
	       (return))
	     (loop
	      (unless (and (or (not (funcall sort-key pivot (aref input-array left-index))) (= pivot (aref input-array left-index))) (< left-index end-index))
		(return))
	      (incf left-index 1))
	     (loop
	      (unless (and (not (funcall sort-key (aref input-array right-index) pivot)) (> right-index begin-index))
		(return))
	      (decf right-index 1))
	     (if (< left-index right-index)
		 (swap-array-items input-array left-index right-index)
	       (return)))
	    (when (> right-index begin-index)
	      (swap-array-items input-array begin-index right-index)
	      (do-quick-sort input-array begin-index (- right-index 1) sort-key))
	    (when (< right-index end-index)
	      (do-quick-sort input-array (+ right-index 1) end-index sort-key)))))))
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length input-array) 1)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (let ((sorting-length (- right left)))
      (when (> sorting-length 1)
	(let ((sequence-to-sort (make-array `(,sorting-length) :displaced-to input-array :displaced-index-offset left)))
	  (counter-sort sequence-to-sort) ; ensure the array elements are shuffled to avoid having sorted sequences prior to passing it to quick sort
	  (do-quick-sort sequence-to-sort 0 (- sorting-length 1) sort-key))))))

(defun heap-sort (input-array &key sort-key left right)
  "This function implements the heap sort algorithm. Heap build is performed prior to actual sorting."
  (defun build-heap (input-array sort-key)
    (dotimes (index (length input-array))
      (let ((checked-element-index index))
	(loop
	 (when (<= checked-element-index 0)
	   (return))
	 (let ((parent-element-index (floor (- checked-element-index 1) 2)))
	   (if (not (funcall sort-key (aref input-array checked-element-index) (aref input-array parent-element-index)))
	       (progn
		 (swap-array-items input-array parent-element-index checked-element-index)
		 (setq checked-element-index parent-element-index))
	     (return)))))))
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  (when (> (length input-array) 1)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (let ((sorting-length (- right left)))
      (when (> sorting-length 1)
	(let ((sequence-to-sort (make-array `(,sorting-length) :displaced-to input-array :displaced-index-offset left)))
	  (build-heap sequence-to-sort sort-key)
	  (let ((current-to-sort-index (- sorting-length 1)))
	    (loop
	     (when (<= current-to-sort-index 0)
	       (return))
	     (swap-array-items sequence-to-sort 0 current-to-sort-index)
	     (decf current-to-sort-index 1)
	     (let ((checked-element-index 0))
	       (loop
		(when (<= current-to-sort-index checked-element-index)
		  (return))
		(let ((left-child-index (+ (* checked-element-index 2) 1)))
		  (cond ((> current-to-sort-index left-child-index)
			 (let ((right-child-index (+ left-child-index 1)) (to-swap-index left-child-index))
			   (unless (funcall sort-key (aref sequence-to-sort right-child-index) (aref sequence-to-sort left-child-index))
			     (setq to-swap-index right-child-index))
			   (cond ((not (funcall sort-key (aref sequence-to-sort to-swap-index) (aref sequence-to-sort checked-element-index)))
				  (swap-array-items sequence-to-sort checked-element-index to-swap-index)
				  (setq checked-element-index to-swap-index))
				 (t (return)))))
			((= left-child-index current-to-sort-index)
			 (unless (funcall sort-key (aref sequence-to-sort left-child-index) (aref sequence-to-sort checked-element-index))
			   (swap-array-items sequence-to-sort checked-element-index left-child-index))
			 (return))
			(t (return)))))))))))))

(defun bucket-sort(input-array &key left right reverse)
  "This function implements a quasi-linear sorting algorithm: bucket sort. For the moment it only deals with integer numbers (possibly to be updated in the future to support real numbers too)."
  (check-type input-array array)
  (unless (null left)
    (check-type left integer)
    (assert (and (>= left 0) (< left (length input-array))) (left) "Left interval index out of range"))
  (unless (null right)
    (check-type right integer)
    (assert (and (> right 0) (<= right (length input-array))) (right) "Right interval index out of range"))
  (unless (or (null left) (null right))
    (assert (< left right) (left right) "Invalid sorting sub-sequence"))
  (dotimes (index (length input-array))
    (check-type (aref input-array index) integer))
  (defconstant +insertion-sort-threshold+ 10 "Maximum number of bucket elements for which insertion sort applies. For more elements quick sort is applied.")
  (when (> (length input-array) 1)
    (when (null left)
      (setq left 0))
    (when (null right)
      (setq right (length input-array)))
    (let ((sorting-length (- right left)))
      (when (> sorting-length 1)
	(let* ((sequence-to-sort (make-array `(,sorting-length) :displaced-to input-array :displaced-index-offset left))
	       (min-max-elements (get-min-max-array-element sequence-to-sort))
	       (offset (- 0 (car min-max-elements))) ; used for retrieving the bucket number (all elements temporarily made positive for this)
	       (range-length (+ (cadr min-max-elements) offset 1)) ; required for retrieving the bucket number (actual number of elements should be matched to the maximum distinct numbers within interval)
	       (bucket-factor (ceiling (/ range-length sorting-length))) ; maximum number of distinct elements to be entered into a bucket
	       (buckets-number (min range-length sorting-length)) ; actual number of buckets used for sorting
	       (buckets (make-array `(,buckets-number)))) ; hash-table containing buckets (by concatenating sorted buckets we get the sorted array)
	  ; distribute elements among buckets
	  (dotimes (index sorting-length)
	    (let ((bucket-number (floor (+ (aref sequence-to-sort index) offset) bucket-factor)))
	      (when reverse
		(setq bucket-number (- (length buckets) 1 bucket-number)))
	      (if (not (null (aref buckets bucket-number)))
		  (setf (aref buckets bucket-number) (cons (aref sequence-to-sort index) (aref buckets bucket-number)))
		(setf (aref buckets bucket-number) (list (aref sequence-to-sort index))))))
	  ; sort each bucket, concatenate them to get the sorted array
	  (let* ((left-index 0) (right-index left-index) (sort-key))
	    (when reverse
	      (setq sort-key (lambda(a b)(let ((result))(setq result (>= a b))))))
	    (dotimes (index (length buckets))
	      (unless (null (aref buckets index))
		(dolist (element (aref buckets index))
		  (setf (aref sequence-to-sort right-index) element)
		  (incf right-index 1))
		(if (<= (- right-index left-index) +insertion-sort-threshold+)
		    (insertion-sort sequence-to-sort :sort-key sort-key :left left-index :right right-index)
		  (quick-sort sequence-to-sort :sort-key sort-key :left left-index :right right-index)))
	      (setq left-index right-index))))))))

(defun get-sorted-groups-info(input-array &optional sort-key)
  "This function provides specific statistics regarding the elements of an array (e.g. number of sub-sequences sorted by key)."
  (defun get-minimum-number-of-sorted-elements(current-min-number provided-min-number)
    (let ((new-min-number))
      (if (= current-min-number 0)
	  (setq new-min-number provided-min-number)
	(setq new-min-number (min current-min-number provided-min-number)))))
  (check-type input-array array)
  (dotimes (index (length input-array))
    (check-type (aref input-array index) (or integer float rational)))
  (if (not (null sort-key))
      (check-type sort-key function)
    (setq sort-key (lambda(a b)(let ((result))(setq result (<= a b))))))
  ; states: 0 - initial, 1 - sorted by key (e.g a>=b), 2 - sorted against key (e.g. a<b)
  (let ((state 0)
	(current-number-of-group-elements 0)
	(groups-sorted-by-key 0)
	(groups-sorted-against-key 0)
	(min-number-of-elements-sorted-by-key 0)
	(max-number-of-elements-sorted-by-key 0)
	(min-number-of-elements-sorted-against-key 0)
	(max-number-of-elements-sorted-against-key 0))
    ; a sorted group should have minimum three continguously sorted elements
    (when (>= (length input-array) 3)
      (dotimes (index (- (length input-array) 1))
	(case state
	      (0 (setq current-number-of-group-elements 2)
		 (cond ((funcall sort-key (aref input-array index) (aref input-array (+ index 1)))
			(setq state 1))
		       (t (setq state 2))))
	      (1 (cond ((funcall sort-key (aref input-array index) (aref input-array (+ index 1)))
			(incf current-number-of-group-elements 1))
		       (t (when (>= current-number-of-group-elements 3)
			    (incf groups-sorted-by-key 1)
			    (setq min-number-of-elements-sorted-by-key (get-minimum-number-of-sorted-elements min-number-of-elements-sorted-by-key current-number-of-group-elements))
			    (setq max-number-of-elements-sorted-by-key (max max-number-of-elements-sorted-by-key current-number-of-group-elements)))
			  (setq current-number-of-group-elements 2)
			  (setq state 2))))
	      (2 (cond ((funcall sort-key (aref input-array index) (aref input-array (+ index 1)))
			(when (>= current-number-of-group-elements 3)
			  (incf groups-sorted-against-key 1)
			  (setq min-number-of-elements-sorted-against-key (get-minimum-number-of-sorted-elements min-number-of-elements-sorted-against-key current-number-of-group-elements))
			  (setq max-number-of-elements-sorted-against-key (max max-number-of-elements-sorted-against-key current-number-of-group-elements)))
			(setq current-number-of-group-elements 2)
			(setq state 1))
		       (t (incf current-number-of-group-elements 1))))))
      ; at the end of the sequence the state might not change anymore, however the last elements might be a sorted group not taken into account (corner case)
      (when (>= current-number-of-group-elements 3)
	(cond ((= state 1)
	       (incf groups-sorted-by-key 1)
	       (setq min-number-of-elements-sorted-by-key (get-minimum-number-of-sorted-elements min-number-of-elements-sorted-by-key current-number-of-group-elements))
	       (setq max-number-of-elements-sorted-by-key (max max-number-of-elements-sorted-by-key current-number-of-group-elements)))
	      (t
	       (incf groups-sorted-against-key 1)
	       (setq min-number-of-elements-sorted-against-key (get-minimum-number-of-sorted-elements min-number-of-elements-sorted-against-key current-number-of-group-elements))
	       (setq max-number-of-elements-sorted-against-key(max max-number-of-elements-sorted-against-key current-number-of-group-elements))))))
    (return-from get-sorted-groups-info
		 (list groups-sorted-by-key groups-sorted-against-key min-number-of-elements-sorted-by-key max-number-of-elements-sorted-by-key
		       min-number-of-elements-sorted-against-key max-number-of-elements-sorted-against-key (length input-array)))))
