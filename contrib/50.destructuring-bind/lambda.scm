(define-library (picrin destructuring-bind)
  (import (picrin base)
          (picrin macro))

  (define-syntax (destructuring-bind formal value . body)
    (cond
     ((identifier? formal)
      #`(let ((#,formal #,value))
          #,@body))
     ((pair? formal)
      #`(let ((value #,value))
          (destructuring-bind #,(car formal) (car value)
            (destructuring-bind #,(cdr formal) (cdr value)
              #,@body))))
     ((vector? formal)
      ;; TODO
      (error "fixme"))
     (else
      #`(if (equal? #,value '#,formal)
            (begin
              #,@body)
            (error "match failure" #,value '#,formal)))))

  (export destructuring-bind))
