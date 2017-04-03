(let ((define-transformer
        (lambda (name transformer)
          (dictionary-set! (macro-objects) name transformer)))
      (the                              ; synonym for #'var
       (lambda (var)
         (make-identifier var default-environment))))
  ;; cache popular identifiers
  (let ((the-core-define (the 'core#define))
        (the-core-lambda (the 'core#lambda))
        (the-core-begin (the 'core#begin))
        (the-core-quote (the 'core#quote))
        (the-core-set! (the 'core#set!))
        (the-core-if (the 'core#if))
        (the-core-define-macro (the 'core#define-macro))
        (the-define (the 'define))
        (the-lambda (the 'lambda))
        (the-begin (the 'begin))
        (the-quote (the 'quote))
        (the-set! (the 'set!))
        (the-if (the 'if))
        (the-define-macro (the 'define-macro)))

    (define-transformer 'quote
      (lambda (form env)
        (if (= (length form) 2)
            `(,the-core-quote ,(cadr form))
            (error "malformed quote" form))))

    (define-transformer 'if
      (lambda (form env)
        (let ((len (length form)))
          (cond
           ((= len 3) `(,@form #undefined))
           ((= len 4) `(,the-core-if . ,(cdr form)))
           (else (error "malformed if" form))))))

    (define-transformer 'begin
      (lambda (form env)
        (let ((len (length form)))
          (cond
           ((= len 1) #undefined)
           ((= len 2) (cadr form))
           ((= len 3) `(,the-core-begin . ,(cdr form)))
           (else `(,the-core-begin ,(cadr form) (,the-begin . ,(cddr form))))))))

    (define-transformer 'set!
      (lambda (form env)
        (if (and (= (length form) 3) (identifier? (cadr form)))
            `(,the-core-set! . ,(cdr form))
            (error "malformed set!" form))))

    (define (check-formal formal)
      (or (null? formal)
          (identifier? formal)
          (and (pair? formal)
               (identifier? (car formal))
               (check-formal (cdr formal)))))

    (define-transformer 'lambda
      (lambda (form env)
        (if (= (length form) 1)
            (error "malformed lambda" form)
            (if (check-formal (cadr form))
                `(,the-core-lambda ,(cadr form) (,the-begin . ,(cddr form)))
                (error "malformed lambda" form)))))

    (define-transformer 'define
      (lambda (form env)
        (let ((len (length form)))
          (if (= len 1)
              (error "malformed define" form)
              (let ((formal (cadr form)))
                (if (identifier? formal)
                    (if (= len 3)
                        `(,the-core-define . ,(cdr form))
                        (error "malformed define" form))
                    (if (pair? formal)
                        `(,the-define ,(car formal) (,the-lambda ,(cdr formal) . ,(cddr form)))
                        (error "define: binding to non-varaible object" form))))))))

    (define-transformer 'define-macro
      (lambda (form env)
        (if (= (length form) 3)
            (if (identifier? (cadr form))
                `(,the-core-define-macro . ,(cdr form))
                (error "define-macro: binding to non-variable object" form))
            (error "malformed define-macro" form))))


    (define-macro define-auxiliary-syntax
      (lambda (form _)
        `(define-transformer ',(cadr form)
           (lambda _
             (error "invalid use of auxiliary syntax" ',(cadr form))))))

    (define-auxiliary-syntax else)
    (define-auxiliary-syntax =>)
    (define-auxiliary-syntax unquote)
    (define-auxiliary-syntax unquote-splicing)

    (define-transformer 'let
      (lambda (form env)
        (if (identifier? (cadr form))
            (let ((name   (car (cdr form)))
                  (formal (car (cdr (cdr form))))
                  (body   (cdr (cdr (cdr form)))))
              `((,the-lambda ()
                             (,the-define (,name . ,(map car formal)) . ,body)
                             (,name . ,(map cadr formal)))))
            (let ((formal (car (cdr form)))
                  (body   (cdr (cdr form))))
              `((,the-lambda ,(map car formal) . ,body) . ,(map cadr formal))))))

    (define-transformer 'and
      (lambda (form env)
        (if (null? (cdr form))
            #t
            (if (null? (cddr form))
                (cadr form)
                `(,the-if ,(cadr form) (,(the 'and) . ,(cddr form)) #f)))))

    (define-transformer 'or
      (lambda (form env)
        (if (null? (cdr form))
            #f
            (let ((tmp (make-identifier 'it env))) ; should we use #f as the env for tmp?
              `(,(the 'let) ((,tmp ,(cadr form)))
                (,the-if ,tmp ,tmp (,(the 'or) . ,(cddr form))))))))

    (define-transformer 'cond
      (lambda (form env)
        (let ((clauses (cdr form)))
          (if (null? clauses)
              #undefined
              (let ((clause (car clauses)))
                (if (and (identifier? (car clause))
                         (identifier=? (the 'else) (make-identifier (car clause) env)))
                    `(,the-begin . ,(cdr clause))
                    (if (null? (cdr clause))
                        `(,(the 'or) ,(car clause) (,(the 'cond) . ,(cdr clauses)))
                        (if (and (identifier? (cadr clause))
                                 (identifier=? (the '=>) (make-identifier (cadr clause) env)))
                            (let ((tmp (make-identifier 'tmp env)))
                              `(,(the 'let) ((,tmp ,(car clause)))
                                (,the-if ,tmp (,(cadr (cdr clause)) ,tmp) (,(the 'cond) . ,(cddr form)))))
                            `(,the-if ,(car clause)
                                      (,the-begin . ,(cdr clause))
                                      (,(the 'cond) . ,(cdr clauses)))))))))))

    (define-transformer 'quasiquote
      (lambda (form env)

        (define (quasiquote? form)
          (and (pair? form)
               (identifier? (car form))
               (identifier=? (the 'quasiquote) (make-identifier (car form) env))))

        (define (unquote? form)
          (and (pair? form)
               (identifier? (car form))
               (identifier=? (the 'unquote) (make-identifier (car form) env))))

        (define (unquote-splicing? form)
          (and (pair? form)
               (pair? (car form))
               (identifier? (caar form))
               (identifier=? (the 'unquote-splicing) (make-identifier (caar form) env))))

        (define (qq depth expr)
          (cond
           ;; unquote
           ((unquote? expr)
            (if (= depth 1)
                (cadr expr)
                (list (the 'list)
                      (list (the 'quote) (the 'unquote))
                      (qq (- depth 1) (car (cdr expr))))))
           ;; unquote-splicing
           ((unquote-splicing? expr)
            (if (= depth 1)
                (list (the 'append)
                      (car (cdr (car expr)))
                      (qq depth (cdr expr)))
                (list (the 'cons)
                      (list (the 'list)
                            (list (the 'quote) (the 'unquote-splicing))
                            (qq (- depth 1) (car (cdr (car expr)))))
                      (qq depth (cdr expr)))))
           ;; quasiquote
           ((quasiquote? expr)
            (list (the 'list)
                  (list (the 'quote) (the 'quasiquote))
                  (qq (+ depth 1) (car (cdr expr)))))
           ;; list
           ((pair? expr)
            (list (the 'cons)
                  (qq depth (car expr))
                  (qq depth (cdr expr))))
           ;; vector
           ((vector? expr)
            (list (the 'list->vector) (qq depth (vector->list expr))))
           ;; simple datum
           (else
            (list (the 'quote) expr))))

        (let ((x (cadr form)))
          (qq 1 x))))

    (define-transformer 'let*
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (body     (cdr (cdr form))))
          (if (null? bindings)
              `(,(the 'let) () . ,body)
              `(,(the 'let) ((,(car (car bindings)) . ,(cdr (car bindings))))
                (,(the 'let*) ,(cdr bindings) . ,body))))))

    (define-transformer 'letrec
      (lambda (form env)
        `(,(the 'letrec*) . ,(cdr form))))

    (define-transformer 'letrec*
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (body     (cdr (cdr form))))
          (let ((variables (map (lambda (v) `(,v #undefined)) (map car bindings)))
                (initials  (map (lambda (v) `(,(the 'set!) ,@v)) bindings)))
            `(,(the 'let) ,variables
              ,@initials
              ,@body)))))

    (define-transformer 'let-values
      (lambda (form env)
        `(,(the 'let*-values) ,@(cdr form))))

    (define-transformer 'let*-values
      (lambda (form env)
        (let ((formals (cadr form))
              (body    (cddr form)))
          (if (null? formals)
              `(,(the 'let) () ,@body)
              (let ((formal (car formals)))
                `(,(the 'call-with-values) (,the-lambda () . ,(cdr formal))
                  (,(the 'lambda) ,(car formal)
                   (,(the 'let*-values) ,(cdr formals) . ,body))))))))

    (define-transformer 'define-values
      (lambda (form env)
        (let ((formal (cadr form))
              (body   (cddr form)))
          (let ((tmps (let loop ((formal formal))
                        (if (identifier? formal)
                            (make-identifier formal env)
                            (if (pair? formal)
                                (cons (make-identifier (car formal) env) (loop (cdr formal)))
                                '())))))
            `(,the-begin
              ,@(let loop ((formal formal))
                  (if (identifier? formal)
                      `((,the-define ,formal #undefined))
                      (if (pair? formal)
                          (cons `(,the-define ,(car formal) #undefined) (loop (cdr formal)))
                          '())))
              (,(the 'call-with-values) (,the-lambda () . ,body)
               (,the-lambda ,tmps . ,(let loop ((formal formal) (tmps tmps))
                                       (if (identifier? formal)
                                           `((,the-set! ,formal ,tmps))
                                           (if (pair? formal)
                                               (cons `(,the-set! ,(car formal) ,(car tmps))
                                                     (loop (cdr formal) (cdr tmps)))
                                               '()))))))))))

    (define-transformer 'do
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (test     (car (car (cdr (cdr form)))))
              (cleanup  (cdr (car (cdr (cdr form)))))
              (body     (cdr (cdr (cdr form)))))
          (let ((loop (make-identifier 'loop env)))
            `(,(the 'let) ,loop ,(map (lambda (x) `(,(car x) ,(cadr x))) bindings)
              (,the-if ,test
                       (,the-begin . ,cleanup)
                       (,the-begin
                        ,@body
                        (,loop . ,(map (lambda (x)
                                         (if (null? (cdr (cdr x)))
                                             (car x)
                                             (car (cdr (cdr x)))))
                                       bindings)))))))))

    (define-transformer 'when
      (lambda (form env)
        (let ((test (car (cdr form)))
              (body (cdr (cdr form))))
          `(,the-if ,test
                    (,the-begin ,@body)
                    #undefined))))

    (define-transformer 'unless
      (lambda (form env)
        (let ((test (car (cdr form)))
              (body (cdr (cdr form))))
          `(,the-if ,test
                    #undefined
                    (,the-begin ,@body)))))

    (define-transformer 'case
      (lambda (form env)
        (let ((key     (car (cdr form)))
              (clauses (cdr (cdr form))))
          (let ((the-key (make-identifier 'key env)))
            `(,(the 'let) ((,the-key ,key))
              ,(let loop ((clauses clauses))
                 (if (null? clauses)
                     #undefined
                     (let ((clause (car clauses)))
                       `(,the-if ,(if (and (identifier? (car clause))
                                           (identifier=? (the 'else) (make-identifier (car clause) env)))
                                      #t
                                      `(,(the 'or) ,@(map (lambda (x) `(,(the 'eqv?) ,the-key (,the-quote ,x))) (car clause))))
                                 ,(if (and (identifier? (cadr clause))
                                           (identifier=? (the '=>) (make-identifier (cadr clause) env)))
                                      `(,(car (cdr (cdr clause))) ,the-key)
                                      `(,the-begin ,@(cdr clause)))
                                 ,(loop (cdr clauses)))))))))))

    (define-transformer 'parameterize
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          `(,(the 'with-dynamic-environment)
            (,(the 'list) ,@(map (lambda (x) `(,(the 'cons) ,(car x) ,(cadr x))) formal))
            (,the-lambda () ,@body)))))))
