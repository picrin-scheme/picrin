(define-library (picrin control option)
  (import (scheme base)
          (picrin control)
          (picrin procedure))

  (define unit identity)

  (define (bind m f)
    (and m (f m)))

  (define-syntax reify
    (syntax-rules ()
      ((_ expr)
       (reset (unit expr)))))

  (define (reflect m)
    (shift k (bind m k)))

  (export unit
          bind
          reify
          reflect))
