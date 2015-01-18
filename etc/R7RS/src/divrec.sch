;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.
 
(import (rnrs base)
        (rnrs control)
        (rnrs io simple))

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))
  
(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (ll (create-n (hide count input1)))
         (name "divrec"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (recursive-div2 ll))
     (lambda (result) (equal? (length result) output)))))
