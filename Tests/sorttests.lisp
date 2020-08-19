(load "../Libs/sort.lisp")

(define-test test-counter-sort
  (let ((testArray))
    (setq testArray (make-array '(10) :initial-contents '(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 6 2 4 3 9 5 8 0 7)))

    (setq testArray (make-array '(10) :initial-contents '(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray t (lambda(a b)(let ((result))(setq result (<= a b))))) ; actually the lambda is not required here, default one is sufficient (added only for illustration purpose)
    (assert-true (equalp testArray #(4 1 6 2 5 3 9 7 8 0)))

    (setq testArray (make-array '(10) :initial-contents '(4 1 2 2 3 5 9 5 8 0)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 4 2 3 2 9 5 8 0 5)))

    (setq testArray (make-array '(10) :initial-contents '(4 1 2 2 3 5 9 5 8 0)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(4 1 2 2 5 3 9 5 8 0)))

    (setq testArray (make-array '(10) :initial-contents '(1 2 2 3 4 5 6 7 7 8)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 2 2 4 3 6 5 7 7 8)))

    (setq testArray (make-array '(10) :initial-contents '(1 2 2 3 4 5 6 7 7 8)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(2 1 3 2 5 4 7 6 8 7)))

    (setq testArray (make-array '(10) :initial-contents '(8 7 7 6 5 4 3 2 2 1)))
    (counterSort testArray)
    (assert-true (equalp testArray #(7 8 6 7 4 5 2 3 1 2)))

    (setq testArray (make-array '(10) :initial-contents '(8 7 7 6 5 4 3 2 2 1)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(8 7 7 5 6 3 4 2 2 1)))

    ; less tests with fraction and float (the behavior should be essentially the same)

    (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1.4 2.1 -0.6 3.0 -2.5 9.34 -7.4 8.25 0.01 2.100)))

    (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(2.1 -0.6 2.100 1.4 3.0 -2.5 9.34 -7.4 8.25 0.01)))

    (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counterSort testArray)
    (assert-true (equalp testArray #(-1/2 1/4 -2/4 5/2 -2/7 5/3 9/8 9/8 1/3 4/5)))

    (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(1/4 -1/2 5/3 -2/4 5/2 -2/7 9/8 1/3 9/8 4/5)))

    ; mixed tests with integer, fraction, float

    (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counterSort testArray)
    (assert-true (equalp testArray #(-2 1/4 -2.0 2.5 5/3 4 9/4 2.25 -1 5)))

    (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(1/4 -2 5/3 -2.0 4 9/4 2.5 -1 5 2.25)))
))

(define-test test-bubble-sort
  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (bubbleSort testArray)
  (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8)))

  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (bubbleSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (bubbleSort testArray)
  (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (bubbleSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (bubbleSort testArray)
  (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (bubbleSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (bubbleSort testArray)
  (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (bubbleSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0)))
)

(define-test test-merge-sort
  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (mergeSort testArray)
  (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8)))

  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (mergeSort testArray)
  (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (mergeSort testArray)
  (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (mergeSort testArray)
  (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0)))
)

(define-test test-quick-sort
  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (quickSort testArray)
  (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8)))

  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (quickSort testArray)
  (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (quickSort testArray)
  (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (quickSort testArray)
  (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0)))
)

(define-test test-heap-sort
  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (heapSort testArray)
  (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8)))

  (setq testArray (make-array '(10) :initial-contents '(2 -1 3 0 4 2 5 5 -7 8)))
  (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (heapSort testArray)
  (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
  (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (heapSort testArray)
  (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34)))

  (setq testArray (make-array '(10) :initial-contents '(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
  (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (heapSort testArray)
  (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5)))

  (setq testArray (make-array '(10) :initial-contents '(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
  (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
  (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0)))
)

(define-test test-sorted-groups-info
  (assert-true (equalp (getSortedGroupsInfo #(-2 4  1 3 5 8 5 -1  2 1 4 2 3  -7 4 5  -1  9 7 -2)) '(2 2 3 4 3 3 20)))
  (assert-true (equalp (getSortedGroupsInfo #(-2 4  1 3 5 8 5 -1  2 1 4 2 3  -7 4 5  -1  9 7 -2) (lambda(a b)(let ((result))(setq result (>= a b))))) '(2 2 3 3 3 4 20)))
  (assert-true (equalp (getSortedGroupsInfo #(1 -1 -1 5 4 3 1 -2 4 8 9 10 11 5 3 3 2 4 6 7 -5 2 4 2 1 10 12 19 21 23 -5 -4 3 0 0 0 3 7 4 12 14 -5 -3 2 0 1 -3 2 5 5)) '(10 3 3 6 3 5 50)))
  (assert-true (equalp (getSortedGroupsInfo #(1 -1 -1 5 4 3 1 -2 4 8 9 10 11 5 3 3 2 4 6 7 -5 2 4 2 1 10 12 19 21 23 -5 -4 3 0 0 0 3 7 4 12 14 -5 -3 2 0 1 -3 2 5 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(5 9 3 5 3 6 50)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 4 5 3 2 -1 -2 9 10 14 15 20 9 3 1 -2 -6 -10 -14)) '(2 2 4 6 5 8 20)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 4 5 3 2 -1 -2 9 10 14 15 20 9 3 1 -2 -6 -10 -14) (lambda(a b)(let ((result))(setq result (>= a b))))) '(2 2 5 8 4 6 20)))
  (assert-true (equalp (getSortedGroupsInfo #(2 -1 5)) '(0 0 0 0 0 0 3)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 4 5 9)) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 4 5 9) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 5 5 5)))
  (assert-true (equalp (getSortedGroupsInfo #(9 5 4 2 -1)) '(0 1 0 0 5 5 5)))
  (assert-true (equalp (getSortedGroupsInfo #(9 5 4 2 -1) (lambda(a b)(let ((result))(setq result (>= a b))))) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 2 5 9)) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (getSortedGroupsInfo #(-1 2 2 5 9) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 3 3 5)))
  (assert-true (equalp (getSortedGroupsInfo #(9 5 2 2 -1)) '(0 1 0 0 3 3 5)))
  (assert-true (equalp (getSortedGroupsInfo #(9 5 2 2 -1) (lambda(a b)(let ((result))(setq result (>= a b))))) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (getSortedGroupsInfo #(2 -1 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 3)))
  (assert-true (equalp (getSortedGroupsInfo #(2 3 5)) '(1 0 3 3 0 0 3)))
  (assert-true (equalp (getSortedGroupsInfo #(2 3 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 3 3 3)))
  (assert-true (equalp (getSortedGroupsInfo #(2 3)) '(0 0 0 0 0 0 2)))
  (assert-true (equalp (getSortedGroupsInfo #(2 3) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 2)))
  (assert-true (equalp (getSortedGroupsInfo #(2)) '(0 0 0 0 0 0 1)))
  (assert-true (equalp (getSortedGroupsInfo #(2) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 1)))
  (assert-true (equalp (getSortedGroupsInfo #()) '(0 0 0 0 0 0 0)))
  (assert-true (equalp (getSortedGroupsInfo #() (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 0)))
)
