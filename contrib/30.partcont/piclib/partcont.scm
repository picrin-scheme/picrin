(define-library (picrin control)
  (import (scheme base))

  ; based on paper "Representing Monads" (Filinski 1994)

  (define m #f)

  (define (abort t)
    (let ((v (t)))                      ; (t) may update m. do not place me like (m (t))
      (m v)))

  (define (reset t)
    (let ((n m))
      (call/cc
       (lambda (k)
         (set! m (lambda (r)
                   (set! m n)
                   (k r)))
         (abort t)))))

  (define (shift h)
    (call/cc
     (lambda (k)
       (abort
        (lambda ()
          (h (lambda (v)
               (reset (lambda ()
                        (k v))))))))))

  (define-syntax reset*
    (syntax-rules ()
      ((_ expr ...)
       (reset (lambda () expr ...)))))

  (define-syntax shift*
    (syntax-rules ()
      ((_ k expr ...)
       (shift (lambda (k) expr ...)))))

  (export (rename shift* shift)
          (rename reset* reset)))

