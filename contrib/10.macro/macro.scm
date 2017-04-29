(define-library (picrin macro)
  (import (picrin base))

  ;; macro primitives

  (export define-macro
          make-identifier
          identifier?
          identifier=?
          identifier-base
          identifier-environment)

  ;; simple macro

  (export define-syntax
          let-syntax letrec-syntax
          syntax-quote
          syntax-quasiquote
          syntax-unquote
          syntax-unquote-splicing)

  ;; other transformers

  (export call-with-current-environment
          make-syntactic-closure
          close-syntax
          strip-syntax
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer
          ir-macro-transformer)


  ;; environment extraction


  (define-macro call-with-current-environment
    (lambda (form env)
      `(,(cadr form) ',env)))


  ;; simple macro


  (define-macro define-auxiliary-syntax
    (lambda (form _)
      `(define-macro ,(cadr form)
         (lambda _
           (error "invalid use of auxiliary syntax" ',(cadr form))))))

  (define-auxiliary-syntax syntax-unquote)
  (define-auxiliary-syntax syntax-unquote-splicing)

  (define (transformer f)
    (lambda (form env)
      (let ((attr1 (make-attribute))
            (attr2 (make-attribute)))
        (letrec
            ((wrap (lambda (var1)
                     (or (attr1 var1)
                         (let ((var2 (make-identifier var1 env)))
                           (attr1 var1 var2)
                           (attr2 var2 var1)
                           var2))))
             (unwrap (lambda (var2)
                       (or (attr2 var2)
                           var2)))
             (walk (lambda (f form)
                     (cond
                      ((identifier? form)
                       (f form))
                      ((pair? form)
                       (cons (walk f (car form)) (walk f (cdr form))))
                      (else
                       form)))))
          (let ((form (cdr form)))
            (walk unwrap (apply f (walk wrap form))))))))

  (define (the var)
    (call-with-current-environment
     (lambda (env)
       (make-identifier var env))))

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

  (define-macro define-syntax
    (lambda (form env)
      (let ((formal (car (cdr form)))
            (body   (cdr (cdr form))))
        (if (pair? formal)
            `(,(the 'define-syntax) ,(car formal) (,(the 'lambda) ,(cdr formal) ,@body))
            `(,(the 'define-macro) ,formal (,(the 'transformer) (,(the 'begin) ,@body)))))))

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


  ;; syntactic closure


  (define (make-syntactic-closure env free form)
    (letrec
        ((wrap (let ((attr (make-attribute)))
                 (lambda (var)
                   (or (attr var)
                       (let ((id (make-identifier var env)))
                         (attr var id)
                         id)))))
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
      (letrec
          ((f (lambda (var)
                (let loop ((free free))
                  (if (null? free)
                      (wrap var)
                      (if (identifier=? var (car free))
                          var
                          (loop (cdr free))))))))
        (walk f form))))

  (define (close-syntax form env)
    (make-syntactic-closure env '() form))

  (define (strip-syntax form)
    (letrec
        ((unwrap (lambda (var)
                   (identifier-base var)))
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
      (walk unwrap form)))


  ;; transformers


  (define (sc-transformer f)
    (lambda (form use-env mac-env)
      (make-syntactic-closure mac-env '() (f form use-env))))

  (define (rsc-transformer f)
    (lambda (form use-env mac-env)
      (make-syntactic-closure use-env '() (f form mac-env))))

  (define (er-transformer f)
    (lambda (form use-env mac-env)
      (letrec
          ((rename (let ((attr (make-attribute)))
                     (lambda (var)
                       (or (attr var)
                           (let ((id (make-identifier var mac-env)))
                             (attr var id)
                             id)))))
           (compare (lambda (x y)
                      (identifier=?
                       (make-identifier x use-env)
                       (make-identifier y use-env)))))
        (f form rename compare))))

  (define (ir-transformer f)
    (lambda (form use-env mac-env)
      (let ((attr1 (make-attribute))
            (attr2 (make-attribute)))
        (letrec
            ((inject (lambda (var1)
                       (or (attr1 var1)
                           (let ((var2 (make-identifier var1 use-env)))
                             (attr1 var1 var2)
                             (attr2 var2 var1)
                             var2))))
             (rename (let ((attr (make-attribute)))
                       (lambda (var)
                         (or (attr var)
                             (let ((id (make-identifier var mac-env)))
                               (attr var id)
                               id)))))
             (flip (lambda (var2) ; unwrap if injected, wrap if not injected
                     (or (attr2 var2)
                         (rename var2))))
             (walk (lambda (f form)
                     (cond
                      ((identifier? form)
                       (f form))
                      ((pair? form)
                       (cons (walk f (car form)) (walk f (cdr form))))
                      (else
                       form))))
             (compare (lambda (x y)
                        (identifier=?
                         (make-identifier x mac-env)
                         (make-identifier y mac-env)))))
          (walk flip (f (walk inject form) inject compare))))))

  (define-macro sc-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((sc-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro rsc-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((rsc-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro er-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((er-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro ir-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((ir-transformer #,(cadr f)) form use-env #,mac-env)))))
