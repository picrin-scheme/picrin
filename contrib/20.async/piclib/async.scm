(define-library (picrin control async)
  (import (scheme base)
          (picrin control)
          (picrin promise))

 (define (promise-unit x)
   (make-promise
    (lambda (resolve _)
      (resolve x))))

 (define (promise-bind m f)
   (promise-then m f))

 (define-syntax async
   (syntax-rules ()
     ((_ x ...)
      (reset (lambda ()
               (promise-unit (begin x ...)))))))

 (define (await m)
   (shift (lambda (f)
            (promise-bind m f))))

 (export async await))
