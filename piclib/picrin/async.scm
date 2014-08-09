(define-library (picrin async)
  (import (scheme base)
          (picrin promise))

  (define (async-timer ms)
    (make-promise
     (lambda (resolve reject)
       (set-timeout
        (lambda ()
          (resolve #t))
        ms))))

  (export async-timer))
