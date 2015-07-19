(define-library (picrin control list)
  (import (scheme base)
          (picrin control))

  (define unit list)

  (define (bind m f)
    (apply append (map f m)))

  (define-syntax reify
    (syntax-rules ()
      ((_ expr)
       (reset (unit expr)))))

  (define (reflect m)
    (shift k (bind m k)))

  (define zero '())

  (define plus append)

  (export unit
          bind
          zero
          plus
          reify
          reflect
          (rename reify for)
          (rename reflect in)
          (rename unit yield)))
