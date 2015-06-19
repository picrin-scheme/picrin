(define-library (scheme case-lambda)
  (import (scheme base))

  (define (length+ list)
    (if (pair? list)
        (+ 1 (length+ (cdr list)))
        0))

  (define-syntax case-lambda
    (syntax-rules ()
      ((case-lambda (params body0 ...) ...)
       (lambda args
         (let ((len (length args)))
           (letrec-syntax
               ((cl (syntax-rules ()
                      ((cl)
                       (error "no matching clause"))
                      ((cl (formal . body) . rest)
                       (if (if (list? 'formal)
                               (= len (length 'formal))
                               (>= len (length+ 'formal)))
                           (apply (lambda formal . body) args)
                           (cl . rest))))))
             (cl (params body0 ...) ...)))))))

  (export case-lambda))
