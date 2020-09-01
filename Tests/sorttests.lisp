(load "../Libs/sort.lisp")

(define-test test-counter-sort
  (let ((test-array #(1 4 6 2 3 5 9 7 8 0)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(1 6 2 4 3 9 5 8 0 7))))

  (let ((test-array #(1 4 6 2 3 5 9 7 8 0)))
    (counter-sort test-array t (lambda(a b)(let ((result))(setq result (<= a b))))) ; actually the lambda is not required here, default one is sufficient (added only for illustration purpose)
    (assert-true (equalp test-array #(4 1 6 2 5 3 9 7 8 0))))

  (let ((test-array #(4 1 2 2 3 5 9 5 8 0)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(1 4 2 3 2 9 5 8 0 5))))

  (let ((test-array #(4 1 2 2 3 5 9 5 8 0)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(4 1 2 2 5 3 9 5 8 0))))

  (let ((test-array #(1 2 2 3 4 5 6 7 7 8)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(1 2 2 4 3 6 5 7 7 8))))

  (let ((test-array #(1 2 2 3 4 5 6 7 7 8)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(2 1 3 2 5 4 7 6 8 7))))

  (let ((test-array #(8 7 7 6 5 4 3 2 2 1)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(7 8 6 7 4 5 2 3 1 2))))

  (let ((test-array #(8 7 7 6 5 4 3 2 2 1)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(8 7 7 5 6 3 4 2 2 1))))

    ; less tests with fraction and float (the behavior should be essentially the same)

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(1.4 2.1 -0.6 3.0 -2.5 9.34 -7.4 8.25 0.01 2.100))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(2.1 -0.6 2.100 1.4 3.0 -2.5 9.34 -7.4 8.25 0.01))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(-1/2 1/4 -2/4 5/2 -2/7 5/3 9/8 9/8 1/3 4/5))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(1/4 -1/2 5/3 -2/4 5/2 -2/7 9/8 1/3 9/8 4/5))))

    ; mixed tests with integer, fraction, float

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counter-sort test-array)
    (assert-true (equalp test-array #(-2 1/4 -2.0 2.5 5/3 4 9/4 2.25 -1 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (counter-sort test-array t)
    (assert-true (equalp test-array #(1/4 -2 5/3 -2.0 4 9/4 2.5 -1 5 2.25))))
)

(define-test test-shuffle-array
  (let ((test-array #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
	(test-arrayCopy #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)))
    (shuffle-array test-array)
    (assert-false (equalp test-array test-arrayCopy))
    (insertion-sort test-array)
    (assert-true (equalp test-array test-arrayCopy)))

  (let ((test-array #(25 23 22 22 15 13 12 12 12 8 7 5 3 3 3 3 -2 -4 -8 -9 -9 -11 -11 -14 -15))
	(test-arrayCopy #(25 23 22 22 15 13 12 12 12 8 7 5 3 3 3 3 -2 -4 -8 -9 -9 -11 -11 -14 -15)))
    (shuffle-array test-array)
    (assert-false (equalp test-array test-arrayCopy))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array test-arrayCopy)))

  (let ((test-array #(0 -1 2 4 3 9 5 5 2 8 -7 -5 3 2 4 12 8 7 7 6 4 5 10 -2 -2))
	(test-arrayCopy #(0 -1 2 4 3 9 5 5 2 8 -7 -5 3 2 4 12 8 7 7 6 4 5 10 -2 -2)))
    (shuffle-array test-array)
    (assert-false (equalp test-array test-arrayCopy))
    (insertion-sort test-array)
    (insertion-sort test-arrayCopy)
    (assert-true (equalp test-array test-arrayCopy)))

  (let ((test-array #(5)))
    (shuffle-array test-array)
    (assert-true (equalp test-array #(5))))

  (let ((test-array #(5 -2)))
    (shuffle-array test-array)
    (assert-true (equalp test-array #(5 -2))))
)

(define-test test-bubble-sort
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (bubble-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bubble-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bubble-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (bubble-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (bubble-sort test-array)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (bubble-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-insertion-sort
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertion-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (insertion-sort test-array)
    (assert-true (equalp test-array #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (insertion-sort test-array)
    (assert-true (equalp test-array #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (insertion-sort test-array)
    (assert-true (equalp test-array #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (insertion-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (insertion-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (insertion-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (insertion-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (insertion-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (insertion-sort test-array :left 0 :right 1)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (insertion-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-merge-sort
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (merge-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (merge-sort test-array)
    (assert-true (equalp test-array #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (merge-sort test-array)
    (assert-true (equalp test-array #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (merge-sort test-array)
    (assert-true (equalp test-array #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (merge-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (merge-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (merge-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (merge-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (merge-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (merge-sort test-array :left 0 :right 1)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (merge-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-quick-sort
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (quick-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (quick-sort test-array)
    (assert-true (equalp test-array #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (quick-sort test-array)
    (assert-true (equalp test-array #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (quick-sort test-array)
    (assert-true (equalp test-array #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (quick-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (quick-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (quick-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (quick-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (quick-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (quick-sort test-array :left 0 :right 1)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (quick-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-heap-sort
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (heap-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (heap-sort test-array)
    (assert-true (equalp test-array #(-1/2 -2/4 -2/7 1/4 1/3 4/5 9/8 9/8 5/3 5/2))))

  (let ((test-array #(1/4 -1/2 -2/4 5/3 5/2 -2/7 9/8 9/8 1/3 4/5)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5/2 5/3 9/8 9/8 4/5 1/3 1/4 -2/7 -2/4 -1/2))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (heap-sort test-array)
    (assert-true (equalp test-array #(-7.4 -2.5 -0.6 0.01 1.4 2.1 2.100 3.0 8.25 9.34))))

  (let ((test-array #(2.1 1.4 -0.6 2.100 3.0 -2.5 9.34 -7.4 8.25 0.01)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(9.34 8.25 3.0 2.100 2.1 1.4 0.01 -0.6 -2.5 -7.4))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (heap-sort test-array)
    (assert-true (equalp test-array #(-2.0 -2 -1 1/4 5/3 2.25 9/4 2.5 4 5))))

  (let ((test-array #(1/4 -2 -2.0 5/3 2.5 4 9/4 2.25 -1 5)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))))
    (assert-true (equalp test-array #(5 4 2.5 9/4 2.25 5/3 1/4 -1 -2 -2.0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (heap-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (heap-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (heap-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :right 10)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (heap-sort test-array :sort-key (lambda(a b)(let ((result))(setq result (>= a b)))) :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (heap-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (heap-sort test-array :left 0 :right 1)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (heap-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-bucket-sort
  (let ((test-array #(9 5 8 7 3 4 6 2 0 1))) ; same number of elements and numbers within range
    (bucket-sort test-array)
    (assert-true (equalp test-array #(0 1 2 3 4 5 6 7 8 9))))

  (let ((test-array #(9 5 8 7 3 4 6 2 0 1))) ; same number of elements and numbers within range
    (bucket-sort test-array :reverse t)
    (assert-true (equalp test-array #(9 8 7 6 5 4 3 2 1 0))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8))) ; higher range than number of elements
    (bucket-sort test-array)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8))) ; higher range than number of elements
    (bucket-sort test-array :reverse t)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-3 1 0 2 1 1 -3 -2 1 -1 -1 -1 0 1 2 2 -1 1 0 0))) ; lower range than number of elements
    (bucket-sort test-array)
    (assert-true (equalp test-array #(-3 -3 -2 -1 -1 -1 -1 0 0 0 0 1 1 1 1 1 1 2 2 2))))

  (let ((test-array #(-3 1 0 2 1 1 -3 -2 1 -1 -1 -1 0 1 2 2 -1 1 0 0))) ; lower range than number of elements
    (bucket-sort test-array :reverse t)
    (assert-true (equalp test-array #(2 2 2 1 1 1 1 1 1 0 0 0 0 -1 -1 -1 -1 -2 -3 -3))))

  ; do some sub-sequence tests too
  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8)))
    (bucket-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bucket-sort test-array :left 0 :right 10)
    (assert-true (equalp test-array #(-7 -1 0 2 2 3 4 5 5 8 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bucket-sort test-array :left 2 :right 12)
    (assert-true (equalp test-array #(-2 10 -7 -1 0 2 2 3 4 5 5 8))))

  (let ((test-array #(2 -1 3 0 4 2 5 5 -7 8 -2 10)))
    (bucket-sort test-array :right 10 :reverse t)
    (assert-true (equalp test-array #(8 5 5 4 3 2 2 0 -1 -7 -2 10))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8)))
    (bucket-sort test-array :left 2 :reverse t)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7))))

  (let ((test-array #(-2 10 2 -1 3 0 4 2 5 5 -7 8 -10 2)))
    (bucket-sort test-array :left 2 :right 12 :reverse t)
    (assert-true (equalp test-array #(-2 10 8 5 5 4 3 2 2 0 -1 -7 -10 2))))

  ; corner cases
  (let ((test-array #(5 -2)))
    (bucket-sort test-array)
    (assert-true (equalp test-array #(-2 5))))

  (let ((test-array #(2)))
    (bucket-sort test-array :left 0 :right 1)
    (assert-true (equalp test-array #(2))))

  (let ((test-array #()))
    (bucket-sort test-array)
    (assert-true (equalp test-array #())))
)

(define-test test-sorted-groups-info
  (assert-true (equalp (get-sorted-groups-info #(-2 4  1 3 5 8 5 -1  2 1 4 2 3  -7 4 5  -1  9 7 -2)) '(2 2 3 4 3 3 20)))
  (assert-true (equalp (get-sorted-groups-info #(-2 4  1 3 5 8 5 -1  2 1 4 2 3  -7 4 5  -1  9 7 -2) (lambda(a b)(let ((result))(setq result (>= a b))))) '(2 2 3 3 3 4 20)))
  (assert-true (equalp (get-sorted-groups-info #(1 -1 -1 5 4 3 1 -2 4 8 9 10 11 5 3 3 2 4 6 7 -5 2 4 2 1 10 12 19 21 23 -5 -4 3 0 0 0 3 7 4 12 14 -5 -3 2 0 1 -3 2 5 5)) '(10 3 3 6 3 5 50)))
  (assert-true (equalp (get-sorted-groups-info #(1 -1 -1 5 4 3 1 -2 4 8 9 10 11 5 3 3 2 4 6 7 -5 2 4 2 1 10 12 19 21 23 -5 -4 3 0 0 0 3 7 4 12 14 -5 -3 2 0 1 -3 2 5 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(5 9 3 5 3 6 50)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 4 5 3 2 -1 -2 9 10 14 15 20 9 3 1 -2 -6 -10 -14)) '(2 2 4 6 5 8 20)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 4 5 3 2 -1 -2 9 10 14 15 20 9 3 1 -2 -6 -10 -14) (lambda(a b)(let ((result))(setq result (>= a b))))) '(2 2 5 8 4 6 20)))
  (assert-true (equalp (get-sorted-groups-info #(2 -1 5)) '(0 0 0 0 0 0 3)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 4 5 9)) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 4 5 9) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 5 5 5)))
  (assert-true (equalp (get-sorted-groups-info #(9 5 4 2 -1)) '(0 1 0 0 5 5 5)))
  (assert-true (equalp (get-sorted-groups-info #(9 5 4 2 -1) (lambda(a b)(let ((result))(setq result (>= a b))))) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 2 5 9)) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (get-sorted-groups-info #(-1 2 2 5 9) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 3 3 5)))
  (assert-true (equalp (get-sorted-groups-info #(9 5 2 2 -1)) '(0 1 0 0 3 3 5)))
  (assert-true (equalp (get-sorted-groups-info #(9 5 2 2 -1) (lambda(a b)(let ((result))(setq result (>= a b))))) '(1 0 5 5 0 0 5)))
  (assert-true (equalp (get-sorted-groups-info #(2 -1 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 3)))
  (assert-true (equalp (get-sorted-groups-info #(2 3 5)) '(1 0 3 3 0 0 3)))
  (assert-true (equalp (get-sorted-groups-info #(2 3 5) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 1 0 0 3 3 3)))
  (assert-true (equalp (get-sorted-groups-info #(2 3)) '(0 0 0 0 0 0 2)))
  (assert-true (equalp (get-sorted-groups-info #(2 3) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 2)))
  (assert-true (equalp (get-sorted-groups-info #(2)) '(0 0 0 0 0 0 1)))
  (assert-true (equalp (get-sorted-groups-info #(2) (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 1)))
  (assert-true (equalp (get-sorted-groups-info #()) '(0 0 0 0 0 0 0)))
  (assert-true (equalp (get-sorted-groups-info #() (lambda(a b)(let ((result))(setq result (>= a b))))) '(0 0 0 0 0 0 0)))
)
