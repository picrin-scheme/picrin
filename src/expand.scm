(define-library (expand)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme cxr)
          (scheme lazy)
          (srfi 1)
          (srfi 17))

  (export make-identifier
          identifier?
          identifier=?
          identifier-base
          identifier-environment
          add-identifier!
          add-macro!
          expand
          builtin-environment)

  (begin

    ;; util

    (define-syntax inc!
      (syntax-rules ()
        ((inc! n)
         (let ((tmp n))
           (set! n (+ n 1))
           tmp))))

    (define-syntax push!
      (syntax-rules ()
        ((push! obj list)
         (set! list (cons obj list)))))


    ;; identifier

    (define-record-type <identifier>
        (make-identifier id env)
        %identifier?
      (id identifier-base)
      (env identifier-environment))

    (define (identifier? obj)
      (or (symbol? obj) (%identifier? obj)))

    (define (identifier=? id1 id2)
      (cond
       ((and (symbol? id1) (symbol? id2))
        (eq? id1 id2))
       ((and (%identifier? id1) (%identifier? id2))
        (eq? (find-identifier (identifier-base id1) (identifier-environment id1))
             (find-identifier (identifier-base id2) (identifier-environment id2))))
       (else
        #f)))

    (define (identifier-name id)
      (if (symbol? id)
          id
          (identifier-name (identifier-base id))))


    ;; environment

    (define uniq
      (let ((n 0))
        (lambda (id)
          (string->symbol
           (string-append
            (symbol->string (identifier-name id))
            "."
            (number->string (inc! n)))))))

    (define (toplevel env)
      (if (null? (cdr env))
          env
          (toplevel (cdr env))))

    (define (search-scope id env)
      (let ((x (assq id (car env))))
        (if x
            (cdr x)
            #f)))

    (define (search-env id env)
      (if (null? env)
          #f
          (or (search-scope id env)
              (search-env id (cdr env)))))

    (define (find-identifier id env)
      (or (search-env id env)
          (if (symbol? id)
              (add-identifier! id (toplevel env))
              (find-identifier (identifier-base id) (identifier-environment id)))))

    (define (add-identifier! id env)
      (or (search-scope id env)
          (let ((uid (uniq id)))
            (set-car! env `((,id . ,uid) . ,(car env)))
            uid)))

    (define (make-environment env)
      (cons '() env))



    ;; macro

    (define global-macro-table
      '())

    (define (find-macro uid)
      (let ((x (assq uid global-macro-table)))
        (and x (cdr x))))

    (define (add-macro! uid expander)
      (push! `(,uid . ,expander) global-macro-table))

    (define (delete-macro! uid)
      (set! global-macro-table (alist-delete! uid global-macro-table)))


    ;; expander

    (define expand
      (let ((deferred (make-parameter '())))

        (define (map* proc list*)
          (cond
           ((null? list*) list*)
           ((pair? list*) (cons (proc (car list*)) (map* proc (cdr list*))))
           (else (proc list*))))

        (define-syntax defer
          (syntax-rules ()
            ((defer expr)
             (let ((x (cons #f #f)))
               (push! (cons x (delay expr)) (deferred))
               x))))

        (define (resolve)
          (for-each
           (lambda (x)
             (let ((deferred (cdr x)) (skelton (car x)))
               (let ((x (force deferred)))
                 (set-car! skelton (car x))
                 (set-cdr! skelton (cdr x)))))
           (reverse (deferred))))

        (define (literal? x)
          (or (null? x)
              (number? x)
              (boolean? x)
              (string? x)))

        (define (call? x)
          (and (list? x)
               (not (null? x))
               (identifier? (car x))))

        (define (expand-variable var env)
          (let ((x (find-identifier var env)))
            (let ((m (find-macro x)))
              (if m
                  (expand (m var env) env)
                  x))))

        (define (expand-quote obj)
          `(quote ,obj))

        (define (expand-define var form env)
          (let ((uid (add-identifier! var env)))
            (delete-macro! uid)
            `(define ,uid ,(expand form env))))

        (define (expand-lambda args body env)
          (let ((env (make-environment env)))
            (let ((args (map* (lambda (var) (add-identifier! var env)) args)))
              (parameterize ((deferred '()))
                (let ((body (expand body env)))
                  (resolve)
                  `(lambda ,args ,body))))))

        (define (expand-define-macro var transformer env)
          (let ((uid (add-identifier! var env)))
            ;; (eval transformer)
            (let ((expander (print "**TODO**")))
              (add-macro! uid expander)
              #f)))

        (define (expand expr env)
          (cond
           ((literal? expr) (expand-quote expr))
           ((identifier? expr) (expand-variable expr env))
           ((call? expr)
            (let ((functor (find-identifier (first expr) env)))
              (case functor
                ((quote) (expand-quote (second expr)))
                ((define) (expand-define (second expr) (third expr) env))
                ((lambda) (defer (expand-lambda (second expr) (third expr) env)))
                ((define-macro) (expand-define-macro (second expr) (third expr) env))
                (else
                 (let ((m (find-macro functor)))
                   (if m
                       (expand (m expr env) env)
                       (map (lambda (x) (expand x env)) expr)))))))
           ((list? expr)
            (map (lambda (x) (expand x env)) expr))
           (else
            (error "unknown expression type: " expr))))

        (lambda (expr env)
          (let ((x (expand expr env)))
            (resolve)
            x))))

    ;; builtin-environment

    (define builtin-environment
      (list
       `((define . define)
         (lambda . lambda)
         (quote . quote)
         (if . if)
         (set! . set!)
         (begin . begin))))))
