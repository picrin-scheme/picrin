(define-library (scheme eval)
  (import (picrin base)
          (picrin macro))

  (define counter 0)

  (define-syntax (inc! n)
    #`(set! #,n (+ #,n 1)))

  (define (environment . specs)
    (let ((lib (string->symbol
                (string-append "picrin.@@my-environment." (number->string counter)))))
      (inc! counter)
      (make-library lib)
      (parameterize ((current-library lib))
        (eval `(import ,@specs) lib))
      lib))

  (export environment eval))
