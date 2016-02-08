(define-library (scheme eval)
  (import (picrin base))

  (define counter 0)

  (define-syntax (inc! n)
    #`(set! #,n (+ #,n 1)))

  (define (number->symbol n)
    (string->symbol (number->string n)))

  (define (environment . specs)
    (let ((library-name `(picrin @@my-environment ,(number->symbol counter))))
      (inc! counter)
      (let ((lib (make-library library-name)))
        (eval `(import ,@specs) lib)
        lib)))

  (export environment eval))
