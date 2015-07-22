;;; find the most frequently referenced word in the bible.
;;; aziz ghuloum (Nov 2007)
;;; modified (slightly) by Will Clinger (Nov 2007)

(import (rnrs base)
        (rnrs unicode)
        (rnrs sorting)
        (rnrs hashtables)
        (rnrs io simple))

(define (fill input-file h)
  (let ((p (open-input-file input-file)))
    (define (put ls) 
      (hashtable-update! h 
        (string->symbol
          (list->string
            (reverse ls)))
        (lambda (x) (+ x 1))
        0))
    (define (alpha ls) 
      (let ((c (read-char p)))
        (cond
          ((eof-object? c) 
           (put ls))
          ((char-alphabetic? c) 
           (alpha (cons (char-downcase c) ls)))
          (else (put ls) (non-alpha)))))
    (define (non-alpha) 
      (let ((c (read-char p)))
        (cond
          ((eof-object? c) (values))
          ((char-alphabetic? c) 
           (alpha (list (char-downcase c))))
          (else (non-alpha)))))
    (non-alpha)
    (close-input-port p)))

(define (list-head ls n)
  (cond
    ((or (zero? n) (null? ls)) '())
    (else (cons (car ls) (list-head (cdr ls) (- n 1))))))

(define (go input-file)
  (let ((h (make-eq-hashtable)))
    (fill input-file h)
    (let-values (((keys vals) (hashtable-entries h)))
       (let ((ls (map cons 
                      (vector->list keys)
                      (vector->list vals))))
         (list-head 
           (list-sort (lambda (a b) (> (cdr a) (cdr b))) ls)
           10)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "bibfreq"))
    (run-r6rs-benchmark
     (string-append name ":" s2)
     1
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
