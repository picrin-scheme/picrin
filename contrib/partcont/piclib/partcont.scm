(define-library (picrin control)
  (import (scheme base))

  ; based on paper "Representing Monads" (Filinski 1994)

  (define m #f)

  (define (reset t)
    (call/cc
     (lambda (k)
       (let ((n m))
         (set! m (lambda (r)
                   (set! m n)
                   (k r)))
         (t)))))

  (define (shift h)
    (call/cc
     (lambda (k)
       (h (lambda (v)
            (reset (lambda ()
                     (k v))))))))

  (define prompt reset)
  (define control shift)

  (export shift
          reset
          control
          prompt))
