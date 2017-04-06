(begin

  ;; expand

  (define-values (make-identifier
                  identifier?
                  identifier=?
                  identifier-name
                  identifier-environment
                  make-environment
                  default-environment
                  environment?
                  find-identifier
                  add-identifier!
                  set-identifier!
                  macro-objects
                  expand)
    (let ()

      ;; identifier

      (define-record-type identifier
          (make-identifier name env)
          %identifier?
        (name identifier-name)
        (env identifier-environment))

      (define (identifier? obj)
        (or (symbol? obj) (%identifier? obj)))

      (define (identifier=? id1 id2)
        (cond
         ((and (symbol? id1) (symbol? id2))
          (eq? id1 id2))
         ((and (%identifier? id1) (%identifier? id2))
          (eq? (find-identifier (identifier-name id1) (identifier-environment id1))
               (find-identifier (identifier-name id2) (identifier-environment id2))))
         (else
          #f)))

      (set! equal?
            (let ((e? equal?))
              (lambda (x y)
                (if (%identifier? x)
                    (identifier=? x y)
                    (e? x y)))))


      ;; environment

      (define-record-type environment
          (%make-environment parent prefix binding)
          environment?
        (parent  environment-parent)
        (prefix  environment-prefix)
        (binding environment-binding))

      (define (search-scope id env)
        ((environment-binding env) id))

      (define (find-identifier id env)
        (or (search-scope id env)
            (let ((parent (environment-parent env)))
              (if parent
                  (find-identifier id parent)
                  (if (symbol? id)
                      (add-identifier! id env)
                      (find-identifier (identifier-name id)
                                       (identifier-environment id)))))))

      (define add-identifier!
        (let ((uniq
               (let ((n 0))
                 (lambda (id)
                   (let ((m n))
                     (set! n (+ n 1))
                     (string->symbol
                      (string-append
                       "."
                       (symbol->string
                        (let loop ((id id))
                          (if (symbol? id)
                              id
                              (loop (identifier-name id)))))
                       "."
                       (number->string m))))))))
          (lambda (id env)
            (or (search-scope id env)
                (if (and (not (environment-parent env)) (symbol? id))
                    (string->symbol
                     (string-append
                      (environment-prefix env)
                      (symbol->string id)))
                    (let ((uid (uniq id)))
                      (set-identifier! id uid env)
                      uid))))))

      (define (set-identifier! id uid env)
        ((environment-binding env) id uid))

      (define (make-environment prefix)
        (%make-environment #f (symbol->string prefix) (make-ephemeron-table)))

      (define default-environment
        (let ((env (make-environment (string->symbol ""))))
          (for-each
           (lambda (x) (set-identifier! x x env))
           '(core#define
             core#set!
             core#quote
             core#lambda
             core#if
             core#begin
             core#define-macro))
          env))

      (define (extend-environment parent)
        (%make-environment parent #f (make-ephemeron-table)))


      ;; macro

      (define global-macro-table
        (make-dictionary))

      (define (find-macro uid)
        (and (dictionary-has? global-macro-table uid)
             (dictionary-ref global-macro-table uid)))

      (define (add-macro! uid expander) ; TODO warn on redefinition
        (dictionary-set! global-macro-table uid expander))

      (define (shadow-macro! uid)
        (when (dictionary-has? global-macro-table uid)
          (dictionary-delete! global-macro-table uid)))

      (define (macro-objects)
        global-macro-table)


      ;; expander

      (define expand
        (let ((task-queue (make-parameter '())))

          (define (queue task)
            (let ((tmp (cons #f #f)))
              (task-queue `((,tmp . ,task) . ,(task-queue)))
              tmp))

          (define (run-all)
            (for-each
             (lambda (x)
               (let ((task (cdr x)) (skelton (car x)))
                 (let ((x (task)))
                   (set-car! skelton (car x))
                   (set-cdr! skelton (cdr x)))))
             (reverse (task-queue))))

          (define (caddr x) (car (cddr x)))

          (define (map* proc list*)
            (cond
             ((null? list*) list*)
             ((pair? list*) (cons (proc (car list*)) (map* proc (cdr list*))))
             (else (proc list*))))

          (define (literal? x)
            (not (or (identifier? x) (pair? x))))

          (define (call? x)
            (and (list? x)
                 (not (null? x))
                 (identifier? (car x))))

          (define (expand-variable var env)
            (let ((x (find-identifier var env)))
              (let ((m (find-macro x)))
                (if m
                    (expand-node (m var env) env)
                    x))))

          (define (expand-quote obj)
            `(core#quote ,obj))

          (define (expand-define var form env)
            (let ((uid (add-identifier! var env)))
              (shadow-macro! uid)
              `(core#define ,uid ,(expand-node form env))))

          (define (expand-lambda args body env)
            (let ((env (extend-environment env)))
              (let ((args (map* (lambda (var) (add-identifier! var env)) args)))
                (parameterize ((task-queue '()))
                  (let ((body (expand-node body env)))
                    (run-all)
                    `(core#lambda ,args ,body))))))

          (define (expand-define-macro var transformer env)
            (let ((uid (add-identifier! var env)))
              (let ((expander (load (expand transformer env))))
                (add-macro! uid expander)
                #undefined)))

          (define (expand-node expr env)
            (cond
             ((literal? expr) expr)
             ((identifier? expr) (expand-variable expr env))
             ((call? expr)
              (let ((functor (find-identifier (car expr) env)))
                (case functor
                  ((core#quote) (expand-quote (cadr expr)))
                  ((core#define) (expand-define (cadr expr) (caddr expr) env))
                  ((core#lambda) (queue (lambda () (expand-lambda (cadr expr) (caddr expr) env))))
                  ((core#define-macro) (expand-define-macro (cadr expr) (caddr expr) env))
                  (else
                   (let ((m (find-macro functor)))
                     (if m
                         (expand-node (m expr env) env)
                         (map (lambda (x) (expand-node x env)) expr)))))))
             ((list? expr)
              (map (lambda (x) (expand-node x env)) expr))
             (else
              (error "invalid expression" expr))))

          (define (expand expr env)
            (let ((x (expand-node expr env)))
              (run-all)
              x))

          expand))

      (values make-identifier
              identifier?
              identifier=?
              identifier-name
              identifier-environment
              make-environment
              default-environment
              environment?
              find-identifier
              add-identifier!
              set-identifier!
              macro-objects
              expand)))


  ;; built-in macros

  (let ()

    (define (define-transformer name transformer)
      (dictionary-set! (macro-objects) name transformer))

    (define (the var)
      (make-identifier var default-environment))

    (let
        ;; cache popular identifiers
        ((the-core-define (the 'core#define))
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
              (let ((obj (cadr form)))
                (cond
                 ((pair? obj) `(,(the 'cons) (,the-quote ,(car obj)) (,the-quote ,(cdr obj))))
                 ((vector? obj) `(,(the 'vector) . ,(vector->list
                                                     (vector-map (lambda (obj) `(,the-quote ,obj)) obj))))
                 (else `(,the-core-quote ,obj))))
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
              (,the-lambda () ,@body)))))

      (define-transformer 'define-record-type
        (lambda (form env)
          (let ((type (car (cdr form)))
                (ctor (car (cdr (cdr form))))
                (pred (car (cdr (cdr (cdr form)))))
                (fields (cdr (cdr (cdr (cdr form))))))
            `(,the-begin
              (,the-define ,ctor
                           (,(the 'make-record) ',type
                            (,(the 'vector) . ,(map (lambda (field) (if (memq (car field) (cdr ctor)) (car field) #undefined)) fields))))
              (,the-define ,pred
                           (,(the 'lambda) (obj)
                            (,(the 'and) (,(the 'record?) obj) (,(the 'eq?) (,(the 'record-type) obj) ',type))))
              . ,(let loop ((fields fields) (pos 0) (acc '()))
                   (if (null? fields)
                       acc
                       (let ((field (car fields)))
                         (let ((defs `((,the-define (,(cadr field) obj)
                                                    (,the-if (,pred obj)
                                                             (,(the 'vector-ref) (,(the 'record-datum) obj) ,pos)
                                                             (,(the 'error) "record type mismatch" obj ',type)))
                                       . ,(if (null? (cddr field))
                                              '()
                                              `((,the-define (,(car (cddr field)) obj value)
                                                             (,the-if (,pred obj)
                                                                      (,(the 'vector-set!) (,(the 'record-datum) obj) ,pos value)
                                                                      (,(the 'error) "record type mismatch" obj ',type))))))))
                           (loop (cdr fields) (+ pos 1) `(,@defs . ,acc))))))))))))

  ;; compile

  (define-values (compile)
    (let ()

      (define (caddr x) (car (cddr x)))
      (define (cadddr x) (cadr (cddr x)))
      (define (max a b) (if (< a b) b a))
      (define (integer? n) (and (number? n) (exact? n)))

      (define normalize
        (let ((defs (make-parameter '())))

          ;; 1. remove core# prefix from keywords
          ;; 2. eliminates internal definitions by replacing with equivalent let & set!
          ;; 3. transform a var into (ref var)
          ;; 4. wrap raw constants with quote

          ;; TODO: warn redefinition, warn duplicate variables

          (define (normalize e)
            (cond
             ((symbol? e) `(ref ,e))
             ((not (pair? e)) `(quote ,e))
             (else
              (case (car e)
                ((core#quote) `(quote . ,(cdr e)))
                ((core#define)
                 (let ((var (cadr e)) (val (caddr e)))
                   (defs (cons var (defs)))
                   `(set! ,var ,(normalize val))))
                ((core#lambda)
                 (let ((args (cadr e)) (body (caddr e)))
                   (parameterize ((defs '()))
                     (let ((body (normalize body)))
                       (if (null? (defs))
                           `(lambda ,args ,body)
                           `(lambda ,args
                              ((lambda ,(defs) ,body) ,@(map (lambda (_) #f) (defs)))))))))
                ((core#set!) `(set! . ,(map normalize (cdr e))))
                ((core#if) `(if . ,(map normalize (cdr e))))
                ((core#begin) `(begin . ,(map normalize (cdr e))))
                (else
                 (map normalize e))))))

          normalize))


      (define transform
        (let ()

          ;; tail-conscious higher-order CPS transformation

          ;; target language
          ;; E ::= A
          ;;     | (if A E E)
          ;;     | (set! v A E)
          ;;     | (A A ...)
          ;; A ::= (lambda (var ...) E)
          ;;     | (ref v)
          ;;     | (quote x)
          ;;     | (undefined)

          (define uniq
            (let ((n 0))
              (lambda ()
                (set! n (+ n 1))
                (string->symbol
                 (string-append "$" (number->string n))))))

          (define (transform-k e k)
            (case (car e)
              ((ref lambda quote) (k (transform-v e)))
              ((begin) (transform-k (cadr e)
                                    (lambda (_)
                                      (transform-k (caddr e) k))))
              ((set!) (transform-k (caddr e)
                                   (lambda (v)
                                     `(set! ,(cadr e) ,v ,(k '(undefined))))))
              ((if) (let ((v (uniq))
                          (c `(ref ,(uniq))))
                      `((lambda (,c)
                          ,(transform-k (cadr e)
                                        (lambda (x)
                                          `(if ,x
                                               ,(transform-c (caddr e) c)
                                               ,(transform-c (cadddr e) c)))))
                        (lambda (,v) ,(k `(ref ,v))))))
              (else
               (let* ((v (uniq))
                      (c `(lambda (,v) ,(k `(ref ,v)))))
                 (transform-k (car e)
                              (lambda (f)
                                (transform*-k (cdr e)
                                              (lambda (args)
                                                `(,f ,c ,@args)))))))))

          (define (transform*-k es k)
            (if (null? es)
                (k '())
                (transform-k (car es)
                             (lambda (x)
                               (transform*-k (cdr es)
                                             (lambda (xs)
                                               (k (cons x xs))))))))

          (define (transform-c e c)
            (case (car e)
              ((ref lambda quote) `(,c ,(transform-v e)))
              ((begin) (transform-k (cadr e)
                                    (lambda (_)
                                      (transform-c (caddr e) c))))
              ((set!) (transform-k (caddr e)
                                   (lambda (v)
                                     `(set! ,(cadr e) ,v (,c (undefined))))))
              ((if) (if (and (pair? c) (eq? 'lambda (car c)))
                        (let ((k `(ref ,(uniq))))
                          `((lambda (,k)
                              ,(transform-k (cadr e)
                                            (lambda (x)
                                              `(if ,x
                                                   ,(transform-c (caddr e) k)
                                                   ,(transform-c (cadddr e) k)))))
                            ,c))
                        (transform-k (cadr e)
                                     (lambda (x)
                                       `(if ,x
                                            ,(transform-c (caddr e) c)
                                            ,(transform-c (cadddr e) c))))))
              (else
               (transform-k (car e)
                            (lambda (f)
                              (transform*-k (cdr e)
                                            (lambda (args)
                                              `(,f ,c ,@args))))))))

          (define (transform-v e)
            (case (car e)
              ((ref quote) e)
              ((lambda)
               (let ((k (uniq)))
                 `(lambda (,k ,@(cadr e)) ,(transform-c (caddr e) `(ref ,k)))))))

          (lambda (e)
            (let ((k (uniq)))
              `(lambda (,k) ,(transform-c e `(ref ,k)))))))


      (define codegen
        (let ()

          ;; TODO: check range of index/depth/frame_size/irepc/objc

          (define (lookup var env)
            (let up ((depth 0) (env env))
              (if (null? env)
                  `(global ,var)
                  (let loop ((index 1) (binding (car env)))
                    (if (symbol? binding)
                        (if (eq? var binding)
                            `(local ,depth ,index)
                            (up (+ depth 1) (cdr env)))
                        (if (null? binding)
                            (up (+ depth 1) (cdr env))
                            (if (eq? var (car binding))
                                `(local ,depth ,index)
                                (loop (+ index 1) (cdr binding)))))))))

          (define env (make-parameter '()))
          (define code (make-parameter '()))
          (define reps (make-parameter '()))
          (define objs (make-parameter '()))

          (define (emit inst)
            (code (cons inst (code))))

          (define (emit-irep irep)
            (let ((n (length (reps))))
              (reps (cons irep (reps)))
              n))

          (define (emit-objs obj)       ; TODO remove duplicates
            (let ((n (length (objs))))
              (objs (cons obj (objs)))
              n))

          (define make-label
            (let ((n 0))
              (lambda ()
                (let ((m n))
                  (set! n (+ n 1))
                  m))))

          (define (emit-label label)
            (code (cons label (code))))

          (define (codegen-e e)
            (case (car e)
              ((ref lambda quote undefined) (codegen-a e 0))
              ((set!) (begin
                        (codegen-a (caddr e) 0)
                        (let* ((x (lookup (cadr e) (env)))
                               (op (if (eq? 'global (car x)) 'GSET 'LSET)))
                          (emit `(,op 0 . ,(cdr x))))
                        (codegen-e (cadddr e))))
              ((if) (begin
                      (codegen-a (cadr e) 0)
                      (let ((label (make-label)))
                        (emit `(COND 0 ,label))
                        (codegen-e (caddr e))
                        (emit-label label)
                        (codegen-e (cadddr e)))))
              (else (begin
                      (let loop ((i 0) (e e))
                        (unless (null? e)
                          (codegen-a (car e) i)
                          (loop (+ i 1) (cdr e))))
                      (emit `(CALL ,(length e)))))))

          (define (codegen-a e i)
            (case (car e)
              ((ref) (let* ((x (lookup (cadr e) (env)))
                            (op (if (eq? 'global (car x)) 'GREF 'LREF)))
                       (emit `(,op ,i . ,(cdr x)))))
              ((quote) (let ((obj (cadr e)))
                         (cond ((eq? #t obj) (emit `(LOADT ,i)))
                               ((eq? #f obj) (emit `(LOADF ,i)))
                               ((null? obj) (emit `(LOADN ,i)))
                               ((and (integer? obj) (<= -128 obj 127)) (emit `(LOADI ,i ,obj)))
                               (else (let ((n (emit-obj obj)))
                                       (emit `(LOAD ,i ,n)))))))
              ((undefined) (emit `(LOADU ,i)))
              ((lambda) (let ((frame-size
                               (let loop ((e (caddr e)))
                                 (case (car e)
                                   ((ref lambda quote undefined) 1)
                                   ((if) (max (loop (caddr e)) (loop (cadddr e))))
                                   ((set!) (loop (cadddr e)))
                                   (else (+ 1 (length e))))))
                              (argc-varg
                               (let loop ((args (cadr e)) (c 0))
                                 (if (symbol? args)
                                     (cons (+ 1 c) #t)
                                     (if (null? args)
                                         (cons c #f)
                                         (loop (cdr args) (+ 1 c)))))))
                          (let ((irep
                                 (parameterize ((code '())
                                                (env (cons (cadr e) (env)))
                                                (reps '())
                                                (objs '()))
                                   (codegen-e (caddr e))
                                   (list (reverse (code)) (reverse (reps)) (reverse (objs)) argc-varg frame-size))))
                            (let ((n (emit-irep irep)))
                              (emit `(PROC ,i ,n))))))))

          (lambda (e)
            (parameterize ((code '()) (env '()) (reps '()) (objs '()))
              (codegen-e e)
              (car (reps))))))

      (lambda (e)
        (codegen (transform (normalize e))))))


  ;; eval

  (define (eval expr . env)
    (load (expand expr (if (null? env) default-environment (car env))))))

