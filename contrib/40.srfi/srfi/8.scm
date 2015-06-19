(define-library (srfi 8)
  (import (scheme base))

  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body ...)
       (call-with-values (lambda () expression)
         (lambda formals body ...)))))

    (export receive))
