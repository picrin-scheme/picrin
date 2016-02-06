;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Hashtable benchmark.
;
; Tests only eq? and eqv? hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (rnrs hashtables))

; Crude test rig, just for benchmarking.

(define failures '())

(define (report-failure! n)
  (set! failures (cons n failures))
  (display "******** TEST FAILED ******** ")
  (write n)
  (newline))

; The parameter n2 is the number of items to be added to the table
; during the stress phase.

(define (hashtable-eq-tests n2 . rest)
  (call-with-current-continuation
   (lambda (exit)
     (let ((maker (if (null? rest) make-eq-hashtable (car rest)))
           (test (lambda (n passed?)
                   (if (not passed?)
                       (report-failure! n)))))

       (let ((t (maker))
             (not-found (list 'not-found))
             (x1 (string #\a #\b #\c))
             (sym1 'sym1)
             (vec1 (vector 'vec1))
             (pair1 (list -1))
             (n1 1000)             ; population added in first phase
            ;(n2 10000)            ; population added in second phase
             (n3 1000))            ; population added in third phase

         (define (hashtable-get t key)
           (hashtable-ref t key #f))

         (test 1 (eq? not-found (hashtable-ref t x1 not-found)))
         (hashtable-set! t x1 'a)
         (test 2 (eq? 'a (hashtable-get t x1)))
         (hashtable-set! t sym1 'b)
         (test 3 (eq? 'a (hashtable-get t x1)))
         (test 4 (eq? 'b (hashtable-get t sym1)))
         (hashtable-set! t vec1 'c)
         (test 5 (eq? 'a (hashtable-get t x1)))
         (test 6 (eq? 'b (hashtable-get t sym1)))
         (test 7 (eq? 'c (hashtable-get t vec1)))
         (hashtable-set! t n2 'd)
         (test 8 (eq? 'a (hashtable-get t x1)))
         (test 9 (eq? 'b (hashtable-get t sym1)))
         (test 10 (eq? 'c (hashtable-get t vec1)))
         (test 11 (eq? 'd (hashtable-get t n2)))

         (hashtable-set! t pair1 'e)

         (do ((i 0 (+ i 1)))
             ((= i n1))
           (hashtable-set! t (list i) i))
         (test 12 (eq? 'e (hashtable-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n2))
           (if (and #f (zero? (mod i 1000))) (display "."))
           (hashtable-set! t (list i) i))
         (test 13 (eq? 'e (hashtable-get t pair1)))
         (do ((i 0 (+ i 1)))
             ((= i n3))
           (test 14 (eq? 'e (hashtable-get t pair1)))
           (hashtable-set! t (list i) i))
         (test 15 (eq? 'a (hashtable-get t x1)))
         (test 16 (eq? 'b (hashtable-get t sym1)))
         (test 17 (eq? 'c (hashtable-get t vec1)))
         (test 18 (eq? 'd (hashtable-get t n2)))
         (test 19 (eq? 'e (hashtable-get t pair1)))

         (hashtable-size t))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "hashtable0"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (hashtable-eq-tests (hide count input1) make-eq-hashtable)
       (hashtable-eq-tests (hide count input2) make-eqv-hashtable))
     (lambda (result) (and (null? failures) (equal? result output))))))
