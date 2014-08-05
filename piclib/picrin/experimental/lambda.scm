(define-library (picrin experimental lambda)
  (import (rename (scheme base)
                  (lambda lambda%))
          (picrin macro))

  (define uniq
    (let ((counter 0))
      (lambda% ()
        (let ((sym (string->symbol (string-append "lv$" (number->string counter)))))
          (set! counter (+ counter 1))
          sym))))

  (define-syntax lambda
    (ir-macro-transformer
     (lambda% (form inject compare)

       (define (bind val args)
         (cond
          ((symbol? args)
           `(define ,args ,val))
          ((pair? args)
           (let ((a (uniq))
                 (b (uniq)))
             `(begin
                (define ,a (car ,val))
                (define ,b (cdr ,val))
                ,(bind a (car args))
                ,(bind b (cdr args)))))
          ((vector? args)
           ;; TODO
           (error "fixme"))
          (else
           `(unless (equal? ,val ',args)
              (error "match failure" ,val ',args)))))

       (let ((args (car (cdr form)))
             (body (cdr (cdr form))))
         (let ((var (uniq)))
          `(lambda% ,var ,(bind var args) ,@body))))))

  (export lambda))
