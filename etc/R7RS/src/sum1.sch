;;; SUM1 -- One of the Kernighan and Van Wyk benchmarks.

(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(define (sumport port sum-so-far)
  (let ((x (read port)))
    (if (eof-object? x)
        sum-so-far
        (sumport port (fl+ x sum-so-far)))))

(define (sum port)
  (sumport port 0.0))

(define (go input)
  (call-with-input-file input sum))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "sum1"))
    (run-r6rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (fl<=? (flabs (fl- result output)) 1e-9)))))

