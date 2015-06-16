(define-library (scheme eval)
  (import (picrin base))

  (define environment
    (let ((counter 0))
      (lambda specs
        (let ((library-name `(picrin @@my-environment ,(string->symbol (number->string counter)))))
          (set! counter (+ counter 1))
          (eval
           `(define-library ,library-name
              ,@(map (lambda (spec) `(import ,spec)) specs))
           (library-environment (find-library '(scheme base))))
          (library-environment (find-library library-name))))))

  (export environment eval))
