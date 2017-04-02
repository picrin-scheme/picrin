(core#define-macro call-with-current-environment
  (core#lambda (form env)
    (list (cadr form) env)))

(core#define here
  (call-with-current-environment
   (core#lambda (env)
     env)))

(core#define the                     ; synonym for #'var
  (core#lambda (var)
    (make-identifier var here)))


(core#define the-builtin-define (the (core#quote core#define)))
(core#define the-builtin-lambda (the (core#quote core#lambda)))
(core#define the-builtin-begin (the (core#quote core#begin)))
(core#define the-builtin-quote (the (core#quote core#quote)))
(core#define the-builtin-set! (the (core#quote core#set!)))
(core#define the-builtin-if (the (core#quote core#if)))
(core#define the-builtin-define-macro (the (core#quote core#define-macro)))

(core#define the-define (the (core#quote define)))
(core#define the-lambda (the (core#quote lambda)))
(core#define the-begin (the (core#quote begin)))
(core#define the-quote (the (core#quote quote)))
(core#define the-set! (the (core#quote set!)))
(core#define the-if (the (core#quote if)))
(core#define the-define-macro (the (core#quote define-macro)))

(core#define-macro quote
  (core#lambda (form env)
    (core#if (= (length form) 2)
      (list the-builtin-quote (cadr form))
      (error "illegal quote form" form))))

(core#define-macro if
  (core#lambda (form env)
    ((core#lambda (len)
       (core#if (= len 4)
           (cons the-builtin-if (cdr form))
           (core#if (= len 3)
               (list the-builtin-if (list-ref form 1) (list-ref form 2) #undefined)
               (error "illegal if form" form))))
     (length form))))

(core#define-macro begin
  (core#lambda (form env)
    ((core#lambda (len)
       (if (= len 1)
           #undefined
           (if (= len 2)
               (cadr form)
               (if (= len 3)
                   (cons the-builtin-begin (cdr form))
                   (list the-builtin-begin
                         (cadr form)
                         (cons the-begin (cddr form)))))))
     (length form))))

(core#define-macro set!
  (core#lambda (form env)
    (if (= (length form) 3)
        (if (identifier? (cadr form))
            (cons the-builtin-set! (cdr form))
            (error "illegal set! form" form))
        (error "illegal set! form" form))))

(core#define check-formal
  (core#lambda (formal)
    (if (null? formal)
        #t
        (if (identifier? formal)
            #t
            (if (pair? formal)
                (if (identifier? (car formal))
                    (check-formal (cdr formal))
                    #f)
                #f)))))

(core#define-macro lambda
  (core#lambda (form env)
    (if (= (length form) 1)
        (error "illegal lambda form" form)
        (if (check-formal (cadr form))
            (list the-builtin-lambda (cadr form) (cons the-begin (cddr form)))
            (error "illegal lambda form" form)))))

(core#define-macro define
  (lambda (form env)
    ((lambda (len)
       (if (= len 1)
           (error "illegal define form" form)
           (if (identifier? (cadr form))
               (if (= len 3)
                   (cons the-builtin-define (cdr form))
                   (error "illegal define form" form))
               (if (pair? (cadr form))
                   (list the-define
                         (car (cadr form))
                         (cons the-lambda (cons (cdr (cadr form)) (cddr form))))
                   (error "define: binding to non-varaible object" form)))))
     (length form))))

(core#define-macro define-macro
  (lambda (form env)
    (if (= (length form) 3)
        (if (identifier? (cadr form))
            (cons the-builtin-define-macro (cdr form))
            (error "define-macro: binding to non-variable object" form))
        (error "illegal define-macro form" form))))


(define-macro syntax-error
  (lambda (form _)
    (apply error (cdr form))))

(define-macro define-auxiliary-syntax
  (lambda (form _)
    (define message
      (string-append
       "invalid use of auxiliary syntax: '" (symbol->string (cadr form)) "'"))
    (list
     the-define-macro
     (cadr form)
     (list the-lambda '_
           (list (the 'error) message)))))

(define-auxiliary-syntax else)
(define-auxiliary-syntax =>)
(define-auxiliary-syntax unquote)
(define-auxiliary-syntax unquote-splicing)
(define-auxiliary-syntax syntax-unquote)
(define-auxiliary-syntax syntax-unquote-splicing)

(define-macro let
  (lambda (form env)
    (if (identifier? (cadr form))
        (list
         (list the-lambda '()
               (list the-define (cadr form)
                     (cons the-lambda
                           (cons (map car (car (cddr form)))
                                 (cdr (cddr form)))))
               (cons (cadr form) (map cadr (car (cddr form))))))
        (cons
         (cons
          the-lambda
          (cons (map car (cadr form))
                (cddr form)))
         (map cadr (cadr form))))))

(define-macro and
  (lambda (form env)
    (if (null? (cdr form))
        #t
        (if (null? (cddr form))
            (cadr form)
            (list the-if
                  (cadr form)
                  (cons (the 'and) (cddr form))
                  #f)))))

(define-macro or
  (lambda (form env)
    (if (null? (cdr form))
        #f
        (let ((tmp (make-identifier 'it env)))
          (list (the 'let)
                (list (list tmp (cadr form)))
                (list the-if
                      tmp
                      tmp
                      (cons (the 'or) (cddr form))))))))

(define-macro cond
  (lambda (form env)
    (let ((clauses (cdr form)))
      (if (null? clauses)
          #undefined
          (let ((clause (car clauses)))
            (if (and (identifier? (car clause))
                     (identifier=? (the 'else) (make-identifier (car clause) env)))
                (cons the-begin (cdr clause))
                (if (null? (cdr clause))
                    (let ((tmp (make-identifier 'tmp here)))
                      (list (the 'let) (list (list tmp (car clause)))
                            (list the-if tmp tmp (cons (the 'cond) (cdr clauses)))))
                    (if (and (identifier? (cadr clause))
                             (identifier=? (the '=>) (make-identifier (cadr clause) env)))
                        (let ((tmp (make-identifier 'tmp here)))
                          (list (the 'let) (list (list tmp (car clause)))
                                (list the-if tmp
                                      (list (car (cddr clause)) tmp)
                                      (cons (the 'cond) (cdr clauses)))))
                        (list the-if (car clause)
                              (cons the-begin (cdr clause))
                              (cons (the 'cond) (cdr clauses)))))))))))

(define-macro quasiquote
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
            (car (cdr expr))
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

(define-macro let*
  (lambda (form env)
    (let ((bindings (car (cdr form)))
          (body     (cdr (cdr form))))
      (if (null? bindings)
          `(,(the 'let) () ,@body)
          `(,(the 'let) ((,(car (car bindings)) ,@(cdr (car bindings))))
            (,(the 'let*) (,@(cdr bindings))
             ,@body))))))

(define-macro letrec
  (lambda (form env)
    `(,(the 'letrec*) ,@(cdr form))))

(define-macro letrec*
  (lambda (form env)
    (let ((bindings (car (cdr form)))
          (body     (cdr (cdr form))))
      (let ((variables (map (lambda (v) `(,v #f)) (map car bindings)))
            (initials  (map (lambda (v) `(,(the 'set!) ,@v)) bindings)))
        `(,(the 'let) (,@variables)
          ,@initials
          ,@body)))))

(define-macro let-values
  (lambda (form env)
    `(,(the 'let*-values) ,@(cdr form))))

(define-macro let*-values
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      (if (null? formal)
          `(,(the 'let) () ,@body)
          `(,(the 'call-with-values) (,the-lambda () ,@(cdr (car formal)))
            (,(the 'lambda) (,@(car (car formal)))
             (,(the 'let*-values) (,@(cdr formal))
              ,@body)))))))

(define-macro define-values
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      (let ((arguments (make-identifier 'arguments here)))
        `(,the-begin
          ,@(let loop ((formal formal))
              (if (pair? formal)
                  `((,the-define ,(car formal) #undefined) ,@(loop (cdr formal)))
                  (if (identifier? formal)
                      `((,the-define ,formal #undefined))
                      '())))
          (,(the 'call-with-values) (,the-lambda () ,@body)
           (,the-lambda
            ,arguments
            ,@(let loop ((formal formal) (args arguments))
                (if (pair? formal)
                    `((,the-set! ,(car formal) (,(the 'car) ,args)) ,@(loop (cdr formal) `(,(the 'cdr) ,args)))
                    (if (identifier? formal)
                        `((,the-set! ,formal ,args))
                        '()))))))))))

(define-macro do
  (lambda (form env)
    (let ((bindings (car (cdr form)))
          (test     (car (car (cdr (cdr form)))))
          (cleanup  (cdr (car (cdr (cdr form)))))
          (body     (cdr (cdr (cdr form)))))
      (let ((loop (make-identifier 'loop here)))
        `(,(the 'let) ,loop ,(map (lambda (x) `(,(car x) ,(cadr x))) bindings)
          (,the-if ,test
                   (,the-begin
                    ,@cleanup)
                   (,the-begin
                    ,@body
                    (,loop ,@(map (lambda (x) (if (null? (cdr (cdr x))) (car x) (car (cdr (cdr x))))) bindings)))))))))

(define-macro when
  (lambda (form env)
    (let ((test (car (cdr form)))
          (body (cdr (cdr form))))
      `(,the-if ,test
                (,the-begin ,@body)
                #undefined))))

(define-macro unless
  (lambda (form env)
    (let ((test (car (cdr form)))
          (body (cdr (cdr form))))
      `(,the-if ,test
                #undefined
                (,the-begin ,@body)))))

(define-macro case
  (lambda (form env)
    (let ((key     (car (cdr form)))
          (clauses (cdr (cdr form))))
      (let ((the-key (make-identifier 'key here)))
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

(define-macro parameterize
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      `(,(the 'with-dynamic-environment)
        (,(the 'list) ,@(map (lambda (x) `(,(the 'cons) ,(car x) ,(cadr x))) formal))
        (,the-lambda () ,@body)))))

(define-macro syntax-quote
  (lambda (form env)
    (let ((renames '()))
      (letrec
          ((rename (lambda (var)
                     (let ((x (assq var renames)))
                       (if x
                           (cadr x)
                           (begin
                             (set! renames `((,var ,(make-identifier var env) (,(the 'make-identifier) ',var ',env)) . ,renames))
                             (rename var))))))
           (walk (lambda (f form)
                   (cond
                    ((identifier? form)
                     (f form))
                    ((pair? form)
                     `(,(the 'cons) (walk f (car form)) (walk f (cdr form))))
                    ((vector? form)
                     `(,(the 'list->vector) (walk f (vector->list form))))
                    (else
                     `(,(the 'quote) ,form))))))
        (let ((form (walk rename (cadr form))))
          `(,(the 'let)
            ,(map cdr renames)
            ,form))))))

(define-macro syntax-quasiquote
  (lambda (form env)
    (let ((renames '()))
      (letrec
          ((rename (lambda (var)
                     (let ((x (assq var renames)))
                       (if x
                           (cadr x)
                           (begin
                             (set! renames `((,var ,(make-identifier var env) (,(the 'make-identifier) ',var ',env)) . ,renames))
                             (rename var)))))))

        (define (syntax-quasiquote? form)
          (and (pair? form)
               (identifier? (car form))
               (identifier=? (the 'syntax-quasiquote) (make-identifier (car form) env))))

        (define (syntax-unquote? form)
          (and (pair? form)
               (identifier? (car form))
               (identifier=? (the 'syntax-unquote) (make-identifier (car form) env))))

        (define (syntax-unquote-splicing? form)
          (and (pair? form)
               (pair? (car form))
               (identifier? (caar form))
               (identifier=? (the 'syntax-unquote-splicing) (make-identifier (caar form) env))))

        (define (qq depth expr)
          (cond
           ;; syntax-unquote
           ((syntax-unquote? expr)
            (if (= depth 1)
                (car (cdr expr))
                (list (the 'list)
                      (list (the 'quote) (the 'syntax-unquote))
                      (qq (- depth 1) (car (cdr expr))))))
           ;; syntax-unquote-splicing
           ((syntax-unquote-splicing? expr)
            (if (= depth 1)
                (list (the 'append)
                      (car (cdr (car expr)))
                      (qq depth (cdr expr)))
                (list (the 'cons)
                      (list (the 'list)
                            (list (the 'quote) (the 'syntax-unquote-splicing))
                            (qq (- depth 1) (car (cdr (car expr)))))
                      (qq depth (cdr expr)))))
           ;; syntax-quasiquote
           ((syntax-quasiquote? expr)
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
           ;; identifier
           ((identifier? expr)
            (rename expr))
           ;; simple datum
           (else
            (list (the 'quote) expr))))

        (let ((body (qq 1 (cadr form))))
          `(,(the 'let)
            ,(map cdr renames)
            ,body))))))

(define (transformer f)
  (lambda (form env)
    (let ((ephemeron1 (make-ephemeron-table))
          (ephemeron2 (make-ephemeron-table)))
      (letrec
          ((wrap (lambda (var1)
                   (let ((var2 (ephemeron1 var1)))
                     (if var2
                         (cdr var2)
                         (let ((var2 (make-identifier var1 env)))
                           (ephemeron1 var1 var2)
                           (ephemeron2 var2 var1)
                           var2)))))
           (unwrap (lambda (var2)
                     (let ((var1 (ephemeron2 var2)))
                       (if var1
                           (cdr var1)
                           var2))))
           (walk (lambda (f form)
                   (cond
                    ((identifier? form)
                     (f form))
                    ((pair? form)
                     (cons (walk f (car form)) (walk f (cdr form))))
                    ((vector? form)
                     (list->vector (walk f (vector->list form))))
                    (else
                     form)))))
        (let ((form (cdr form)))
          (walk unwrap (apply f (walk wrap form))))))))

(define-macro define-syntax
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      (if (pair? formal)
          `(,(the 'define-syntax) ,(car formal) (,the-lambda ,(cdr formal) ,@body))
          `(,the-define-macro ,formal (,(the 'transformer) (,the-begin ,@body)))))))

(define-macro letrec-syntax
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      `(let ()
         ,@(map (lambda (x)
                  `(,(the 'define-syntax) ,(car x) ,(cadr x)))
                formal)
         ,@body))))

(define-macro let-syntax
  (lambda (form env)
    `(,(the 'letrec-syntax) ,@(cdr form))))
