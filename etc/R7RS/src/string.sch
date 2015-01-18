;;; STRING -- One of the Kernighan and Van Wyk benchmarks.
  
(import (rnrs base)
        (rnrs control)
        (rnrs io simple))

(define s "abcdef")

(define (grow)
  (set! s (string-append "123" s "456" s "789"))
  (set! s (string-append
           (substring s (div (string-length s) 2) (string-length s))
           (substring s 0 (+ 1 (div (string-length s) 2)))))
  s)

(define (trial n)
  (do ((i 0 (+ i 1)))
      ((> (string-length s) n) (string-length s))
    (grow)))

(define (my-try n)
  (do ((i 0 (+ i 1)))
      ((>= i 10) (string-length s))
    (set! s "abcdef")
    (trial n)))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "string"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (my-try (hide count input1)))
     (lambda (result) (equal? result output)))))
