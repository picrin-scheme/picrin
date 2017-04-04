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
                compile
                eval)
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

    (define (add-macro! uid expander)    ; TODO warn on redefinition
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

    ;; compile

    (define (compile expr . env)
      (expand expr (if (null? env) default-environment (car env))))

    ;; eval

    (define (eval expr . env)
      (load (compile expr (if (null? env) default-environment (car env)))))

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
            compile
            eval)))

