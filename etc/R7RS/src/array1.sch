;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

(import (rnrs base)
        (rnrs control)
        (rnrs io simple))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go m n)
  (let loop ((repeat m)
             (result '()))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result)))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "array1"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     1
     (lambda () (go (hide count count) (hide count input1)))
     (lambda (result) (equal? result output)))))
