;;; CTAK -- A version of the TAK procedure that uses continuations.

(import (rnrs base)
        (rnrs io simple))

(define (ctak x y z)
  (call-with-current-continuation
    (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
        (lambda (k)
          (ctak-aux
           k
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- x 1) y z)))
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- y 1) z x)))
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "ctak"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (ctak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))
