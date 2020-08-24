(load "../Libs/sort.lisp")

(define-test test-counter-sort
  (let ((testArray #(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 6 2 4 3 9 5 8 0 7))))

  (let ((testArray #(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray t (lambda(a b)(let ((result))(setq result (<= a b))))) ; actually the lambda is not required here, default one is sufficient (added only for illustration purpose)
    (assert-true (equalp testArray #(4 1 6 2 5 3 9 7 8 0))))

  (let ((testArray #(4 1 2 2 3 5 9 5 8 0)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 4 2 3 2 9 5 8 0 5))))

  (let ((testArray #(4 1 2 2 3 5 9 5 8 0)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(4 1 2 2 5 3 9 5 8 0))))

  (let ((testArray #(1 2 2 3 4 5 6 7 7 8)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 2 2 4 3 6 5 7 7 8))))

  (let ((testArray #(1 2 2 3 4 5 6 7 7 8)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(2 1 3 2 5 4 7 6 8 7))))

  (let ((testArray #(8 7 7 6 5 4 3 2 2 1)))
    (counterSort testArray)
    (assert-true (equalp testArray #(7 8 6 7 4 5 2 3 1 2))))

  (let ((testArray #(8 7 7 6 5 4 3 2 2 1)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(8 7 7 5 6 3 4 2 2 1))))

    ; less tests with fraction and float (the behavior should be essentially the same)

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1.4 2.1 -0.6 3.0 -2.5 9.34 -7.4 8.25 0.01 2.100))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(2.1 -0.6 2.100 1.4 3.0 -2.5 9.34 -7.4 8.25 0.01))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counterSort testArray)
    (assert-true (equalp testArray #(-1/2 1/4 -2/4 5/2 -2/7 5/3 9/8 9/8 1/3 4/5))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(1/4 -1/2 5/3 -2/4 5/2 -2/7 9/8 1/3 9/8 4/5))))

    ; mixed tests with integer, fraction, float

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counterSort testArray)
    (assert-true (equalp testArray #(-2 1/4 -2.0 2.5 5/3 4 9/4 2.25 -1 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counterSort testArray t)
    (assert-true (equalp testArray #(1/4 -2 5/3 -2.0 4 9/4 2.5 -1 5 2.25))))
)

(define-test test-shuffle-array
  (let ((testArray #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
	(testArrayCopy #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)))
    (shuffleArray testArray)
    (assert-false (equalp testArray testArrayCopy))
    (insertionSort testArray)
    (assert-true (equalp testArray testArrayCopy)))

  (let ((testArray #(25 23 22 22 15 13 12 12 12 8 7 5 3 3 3 3 -2 -4 -8 -9 -9 -11 -11 -14 -15))
	(testArrayCopy #(25 23 22 22 15 13 12 12 12 8 7 5 3 3 3 3 -2 -4 -8 -9 -9 -11 -11 -14 -15)))
    (shuffleArray testArray)
    (assert-false (equalp testArray testArrayCopy))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray testArrayCopy)))

  (let ((testArray #(0 -1 2 4 3 9 5 5 2 8 -7 -5 3 2 4 12 8 7 7 6 4 5 10 -2 -2))
	(testArrayCopy #(0 -1 2 4 3 9 5 5 2 8 -7 -5 3 2 4 12 8 7 7 6 4 5 10 -2 -2)))
    (shuffleArray testArray)
    (assert-false (equalp testArray testArrayCopy))
    (insertionSort testArray)
    (insertionSort testArrayCopy)
    (assert-true (equalp testArray testArrayCopy)))

  (let ((testArray #(5)))
    (shuffleArray testArray)
    (assert-true (equalp testArray #(5))))

  (let ((testArray #(5 -2)))
    (shuffleArray testArray)
    (assert-true (equalp testArray #(5 -2))))
)

(define-test test-bubble-sort
  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubbleSort testArray :left 0 :right 10)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bubbleSort testArray :left 0 :right 10)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bubbleSort testArray :left 2 :right 12)
    (assert-true (equalp testArray #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp testArray #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (bubbleSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp testArray #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((testArray #(5 -2)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(-2 5))))

  (let ((testArray #(2)))
    (bubbleSort testArray)
    (assert-true (equalp testArray #(2))))

  (let ((testArray #()))
    (bubbleSort testArray)
    (assert-true (equalp testArray #())))
)

(define-test test-insertion-sort
  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertionSort testArray)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (insertionSort testArray)
    (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (insertionSort testArray)
    (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (insertionSort testArray)
    (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertionSort testArray :left 0 :right 10)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (insertionSort testArray :left 0 :right 10)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (insertionSort testArray :left 2 :right 12)
    (assert-true (equalp testArray #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp testArray #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (insertionSort testArray :sortKey (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp testArray #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((testArray #(5 -2)))
    (insertionSort testArray)
    (assert-true (equalp testArray #(-2 5))))

  (let ((testArray #(2)))
    (insertionSort testArray :left 0 :right 1)
    (assert-true (equalp testArray #(2))))

  (let ((testArray #()))
    (insertionSort testArray)
    (assert-true (equalp testArray #())))
)

(define-test test-merge-sort
  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (mergeSort testArray)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (mergeSort testArray)
    (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (mergeSort testArray)
    (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (mergeSort testArray)
    (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (mergeSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))
)

(define-test test-quick-sort
  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (quickSort testArray)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (quickSort testArray)
    (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (quickSort testArray)
    (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (quickSort testArray)
    (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (quickSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))
)

(define-test test-heap-sort
  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (heapSort testArray)
    (assert-true (equalp testArray #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((testArray #(2 -1 3 0 4 2 5 5 -7 8)))
    (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (heapSort testArray)
    (assert-true (equalp testArray #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((testArray #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (heapSort testArray)
    (assert-true (equalp testArray #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((testArray #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (heapSort testArray)
    (assert-true (equalp testArray #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((testArray #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (heapSort testArray (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp testArray #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))
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
