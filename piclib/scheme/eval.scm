(define-library (scheme eval)
  (import (scheme base))

  (define (null-environment n)
    (if (not (= n 5))
        (error "unsupported environment version" n)
        '(scheme null)))

  (define (scheme-report-environment n)
    (if (not (= n 5))
        (error "unsupported environment version" n)
        '(scheme r5rs)))

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

  (export null-environment
          scheme-report-environment
          environment))
