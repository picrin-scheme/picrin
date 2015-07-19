(define-library (picrin procedure)
  (import (scheme base))
  (export >>
          <<
          constant
          identity)

  (define identity values)

  (define (constant . args)
    (lambda _
      (apply values args)))

  (define (>> . fs)
    (if (null? fs)
        identity
        (let ((f (car fs))
              (g (apply >> (cdr fs))))
          (lambda args
            (call-with-values (lambda () (apply f args))
              (lambda args
                (apply g args)))))))

  (define (<< . fs)
    (apply >> (reverse fs))))
