(define-library (picrin control list)
  (import (scheme base)
          (picrin control)
          (scheme write))

  (define-syntax for
    (syntax-rules ()
      ((_ expr)
       (reset (lambda () expr)))))

  (define (in m)
    (shift (lambda (k)
             (apply append (map k m)))))

  (define (yield x)
    (list x))

  (export for in yield))
