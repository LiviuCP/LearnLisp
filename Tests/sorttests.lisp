(load "../Libs/sort.lisp")

(define-test test-counter-sort
  (let ((testArray))
    (setq testArray (make-array '(10) :initial-contents '(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray)
    (assert-true (equalp testArray #(1 6 2 4 3 9 5 8 0 7)))

    (setq testArray (make-array '(10) :initial-contents '(1 4 6 2 3 5 9 7 8 0)))
    (counterSort testArray t)
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
