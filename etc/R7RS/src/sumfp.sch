;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (fl<? i 0.)
        sum
        (loop (fl- i 1.) (fl+ i sum)))))
 
(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "sumfp"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (run (hide count input1)))
     (lambda (result) (equal? result output)))))
