(import (scheme base)
        (picrin test)
        (picrin big-number)
        (srfi 27))

(define (test-add x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (+ x y)))
      (bigint-underlying (bigint-add a b)))))

(define (test-sub x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (- x y)))
      (bigint-underlying (bigint-sub a b)))))

(define (test-mul x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (* x y)))
      (bigint-underlying (bigint-mul a b)))))

(define (bi-add x y)
  (bigint-underlying
    (bigint-add
      (make-bigint x)
      (make-bigint y))))

(define (test-equal x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
        (equal? x y)
        (bigint-equal? a b))))

(define (test-bigint->number x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test (+ x y)
      (bigint->number (bigint-add a b)))))


(define (rand-int n) (floor (* n (random-real))))
(define (test-random func time)
  (do ((iteration 0 (+ iteration 1)))
    ((>= iteration time) #t)
    (let ((x (rand-int 32535)) (y (rand-int 32535)))
      (func x y))))

(test-random test-add 5)
(test-random test-equal 5)
(test-random test-bigint->number 5)
(test-random test-mul 5)

