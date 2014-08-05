(define-library (picrin base)
  (import (rename (picrin base core) (define define*))
          (picrin base macro)
          (picrin base list)
          (scheme base))

  (define-syntax define
    (lambda (form use-env mac-env)
      (if (symbol? (car (cdr form)))
          (cons (make-identifier 'define* mac-env) (cdr form))
          (cons (make-identifier 'define mac-env)
                (cons (car (car (cdr form)))
                      (cons (cons (make-identifier 'lambda mac-env)
                                  (cons (cdr (car (cdr form)))
                                        (cdr (cdr form))))
                            '()))))))

  (export define
          set!
          quote
          lambda
          if
          begin
          define-syntax))
