;;; FIB -- A classic benchmark, computes fib(n) inefficiently.

(import (scheme base)
        (scheme read)
        (scheme write))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
  
(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fib"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fib (hide count input)))
     (lambda (result) (= result output)))))

(include "src/common.sch")
