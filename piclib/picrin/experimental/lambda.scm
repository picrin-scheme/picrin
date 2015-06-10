(define-library (picrin experimental lambda)
  (import (scheme base)
          (picrin base)
          (picrin macro))

  (define-syntax (destructuring-let formal value . body)
    (cond
     ((variable? formal)
      #`(let ((#,formal #,value))
          #,@body))
     ((pair? formal)
      #`(let ((value #,value))
          (destructuring-let #,(car formal) (car value)
            (destructuring-let #,(cdr formal) (cdr value)
              #,@body))))
     ((vector? formal)
      ;; TODO
      (error "fixme"))
     (else
      #`(if (equal? #,value '#,formal)
            (begin
              #,@body)
            (error "match failure" #,value '#,formal)))))

  (define-syntax (destructuring-lambda formal . body)
    #`(lambda args
        (destructuring-let #,formal args #,@body)))

  (define-syntax (destructuring-define formal . body)
    (if (variable? formal)
        #`(define #,formal #,@body)
        #`(destructuring-define #,(car formal)
            (destructuring-lambda #,(cdr formal)
              #,@body))))

  (export (rename destructuring-let let)
          (rename destructuring-lambda lambda)
          (rename destructuring-define define)))
