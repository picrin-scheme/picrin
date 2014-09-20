(define-library (scheme eval)
  (import (picrin base))

  (define environment
    (let ((counter 0))
      (lambda specs
        (let ((library-name `(picrin @@my-environment ,counter)))
          (set! counter (+ counter 1))
          (eval
           `(define-library ,library-name
              ,@(map (lambda (spec)
                       `(import ,spec))
                     specs))
           '(scheme base))
          library-name))))

  (export environment eval))
