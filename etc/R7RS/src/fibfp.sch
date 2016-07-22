;;; FIBFP -- Computes fib(35) using floating point

(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(define (fibfp n)
  (if (fl<? n 2.)
    n
    (fl+ (fibfp (fl- n 1.))
         (fibfp (fl- n 2.)))))

(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibfp"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibfp (hide count input)))
     (lambda (result) (= result output)))))

