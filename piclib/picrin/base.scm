(define-library (picrin base)
  (import (rename (picrin base core) (define define*))
          (scheme base)
          (picrin macro))

  (define-syntax define
    (lambda (form use-env mac-env)
      (if (symbol? (cadr form))
          (cons (make-identifier 'define* mac-env) (cdr form))
          (list (make-identifier 'define mac-env)
                (car (cadr form))
                (cons (make-identifier 'lambda mac-env)
                      (cons (cdr (cadr form))
                            (cddr form)))))))

  (export define
          set!
          quote
          lambda
          if
          begin
          define-syntax))
