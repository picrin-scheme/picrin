(define-library (picrin documentation)
  (import
   (scheme base)
   (srfi 17)
   (picrin dictionary)
   (picrin macro)
   (only (picrin base) attribute))

  (define (documentation proc)
           (dictionary-ref (attribute proc) 'documentation))

  (set! (setter documentation)
        (lambda (proc doc)
          (dictionary-set! (attribute proc) 'documentation doc)))


  (define-syntax %lambda
    (er-macro-transformer 
     (lambda (expr r c)
       (let ((params (cadr expr))
             (body (cddr expr)))
         (if (and (pair? body) (string? (car body))) 
             `(let ((proc (,(r 'lambda) ,params ,@(cdr body))))
                (,(r 'set!) (,(r 'documentation) proc) ,(car body))
                proc)
             `(,(r 'lambda) ,params ,@body))))))

  (define-syntax %define
    (syntax-rules ()
      ((_ var val)
       (define var val))
      ((_ (var arg ...) body ...)
       (define var (%lambda (arg ...) body ...)))))

  
  ;; for sub projects
  (define-syntax doc
    (er-macro-transformer
     (lambda (expr r c)
       (cons (r 'begin)
             (let ((gen (lambda (proc str) `(,(r 'set!) (,(r 'documentation) ,proc) ,str))))
               (let aux ((acc ()) (args (cdr expr)))
                 (if (null? args)
                     (reverse acc)
                     (aux (cons (gen (car args) (cadr args)) acc) (cddr args)))))))))
  
  (export documentation
          (rename %define define)
          (rename %lambda lambda)))
