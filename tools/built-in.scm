(lambda ()
  (let ((transformers '()))

    (define here
      (library-environment "picrin.base"))

    (define (the var)
      (make-identifier var here))

    (define-macro define-transformer
      (lambda (form env)
        (let ((name        (car (cdr form)))
              (transformer (car (cdr (cdr form)))))
          `(set! transformers `((,',name . ,,transformer) . ,transformers)))))

    (define-transformer quote
      (lambda (form env)
        (if (= (length form) 2)
            `(,(the 'builtin:quote) ,(cadr form))
            (error "illegal quote form" form))))

    (define-transformer if
      (lambda (form env)
        (case (length form)
          ((4) `(,(the 'builtin:if) ,@(cdr form)))
          ((3) `(,(the 'builtin:if) ,@(cdr form) #undefined))
          (else (error "illegal if form" form)))))

    (define-transformer begin
      (lambda (form env)
        (case (length form)
          ((1) #undefined)
          ((2) (cadr form))
          ((3) `(,(the 'builtin:begin) ,@(cdr form)))
          (else `(,(the 'builtin:begin) ,(cadr form) (,(the 'begin) ,@(cddr form)))))))

    (define (check-formal formal)
      (or (identifier? formal)
          (null? formal)
          (and (pair? formal)
               (identifier? (car formal))
               (check-formal (cdr formal)))))

    (define-transformer lambda
      (lambda (form env)
        (if (and (>= (length form) 2) (check-formal (cadr form)))
            `(,(the 'builtin:lambda) ,(cadr form) (,(the 'begin) ,@(cddr form)))
            (error "illegal lambda form" form))))

    (define-transformer set!
      (lambda (form env)
        (if (and (= (length form) 3) (identifier? (cadr form)))
            `(,(the 'builtin:set!) ,@(cdr form))
            (error "illegal set! form" form))))

    (define-transformer define
      (lambda (form env)
        (cond
         ((and (= (length form) 3) (identifier? (cadr form)))
          `(,(the 'builtin:define) ,@(cdr form)))
         ((and (>= (length form) 2) (pair? (cadr form)))
          (let ((formal (cadr form)))
            `(,(the 'define) ,(car formal) (,(the 'lambda) ,(cdr formal) ,@(cddr form)))))
         (else
          (error "illegal define form" form)))))

    (define-transformer define-macro
      (lambda (form env)
        (if (and (= (length form) 3) (identifier? (cadr form)))
            `(,(the 'builtin:define-macro) ,@(cdr form))
            (error "illegal define-macro form" form))))

    (define-transformer syntax-error
      (lambda (form _)
        (apply error (cdr form))))

    (define-transformer let
      (lambda (form env)
        (if (identifier? (cadr form))
            `(,(the 'let) ()
              (,(the 'define) (,(cadr form) ,@(map car (car (cddr form))))
               ,@(cdr (cddr form)))
              (,(cadr form) ,@(map cadr (car (cddr form)))))
            `((,(the 'lambda) ,(map car (cadr form)) ,@(cddr form))
              ,@(map cadr (cadr form))))))

    (define-transformer and
      (lambda (form env)
        (cond
         ((= (length form) 1) #t)
         ((= (length form) 2) (cadr form))
         (else `(,(the 'if) ,(cadr form) (,(the 'and) ,@(cddr form)) #f)))))

    (define-transformer or
      (lambda (form env)
        (cond
         ((= (length form) 1) #f)
         (else (let ((tmp (make-identifier 'tmp env)))
                 `(,(the 'let) ((,tmp ,(cadr form)))
                   (,(the 'if) ,tmp ,tmp (,(the 'or) ,@(cddr form)))))))))

    (define-transformer cond
      (lambda (form env)
        (let ((clauses (cdr form)))
          (if (null? clauses)
              #undefined
              (let ((clause (car clauses)))
                (cond
                 ((and (identifier? (car clause))
                       (identifier=? (the 'else) (make-identifier (car clause) env)))
                  `(,(the 'begin) ,@(cdr clause)))
                 ((and (identifier? (cadr clause))
                       (identifier=? (the '=>) (make-identifier (cadr clause) env)))
                  (let ((tmp (make-identifier 'tmp here)))
                    `(,(the 'let) ((,tmp ,(car clause)))
                      (,(the 'if) ,tmp
                       (,(car (cddr clause)) ,tmp)
                       (,(the 'cond) ,@(cdr clauses))))))
                 ((null? (cdr clause))
                  (let ((tmp (make-identifier 'tmp here)))
                    `(,(the 'let) ((,tmp ,(car clause)))
                      (,(the 'if) ,tmp ,tmp (,(the 'cond) ,@(cdr clauses))))))
                 (else
                  `(,(the 'if) ,(car clause)
                    (,(the 'begin) ,@(cdr clause))
                    (,(the 'cond) ,@(cdr clauses))))))))))

    (define-transformer quasiquote
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

    (define-transformer let*
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (body     (cdr (cdr form))))
          (if (null? bindings)
              `(,(the 'let) () ,@body)
              `(,(the 'let) ((,(car (car bindings)) ,@(cdr (car bindings))))
                (,(the 'let*) (,@(cdr bindings))
                 ,@body))))))

    (define-transformer letrec
      (lambda (form env)
        `(,(the 'letrec*) ,@(cdr form))))

    (define-transformer letrec*
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (body     (cdr (cdr form))))
          (let ((variables (map (lambda (v) `(,v #f)) (map car bindings)))
                (initials  (map (lambda (v) `(,(the 'set!) ,@v)) bindings)))
            `(,(the 'let) (,@variables)
              ,@initials
              ,@body)))))

    (define-transformer let-values
      (lambda (form env)
        `(,(the 'let*-values) ,@(cdr form))))

    (define-transformer let*-values
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          (if (null? formal)
              `(,(the 'let) () ,@body)
              `(,(the 'call-with-values) (,(the 'lambda) () ,@(cdr (car formal)))
                (,(the 'lambda) (,@(car (car formal)))
                 (,(the 'let*-values) (,@(cdr formal))
                  ,@body)))))))

    (define-transformer define-values
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          (let ((arguments (make-identifier 'arguments here)))
            `(,(the 'begin)
              ,@(let loop ((formal formal))
                  (if (pair? formal)
                      `((,(the 'define) ,(car formal) #undefined) ,@(loop (cdr formal)))
                      (if (identifier? formal)
                          `((,(the 'define) ,formal #undefined))
                          '())))
              (,(the 'call-with-values) (,(the 'lambda) () ,@body)
               (,(the 'lambda)
                ,arguments
                ,@(let loop ((formal formal) (args arguments))
                    (if (pair? formal)
                        `((,(the 'set!) ,(car formal) (,(the 'car) ,args))
                          ,@(loop (cdr formal) `(,(the 'cdr) ,args)))
                        (if (identifier? formal)
                            `((,(the 'set!) ,formal ,args))
                            '()))))))))))

    (define-transformer do
      (lambda (form env)
        (let ((bindings (car (cdr form)))
              (test     (car (car (cdr (cdr form)))))
              (cleanup  (cdr (car (cdr (cdr form)))))
              (body     (cdr (cdr (cdr form)))))
          (let ((loop (make-identifier 'loop here)))
            `(,(the 'let) ,loop ,(map (lambda (x) `(,(car x) ,(cadr x))) bindings)
              (,(the 'if) ,test
               (,(the 'begin)
                ,@cleanup)
               (,(the 'begin)
                ,@body
                (,loop ,@(map (lambda (x)
                                (if (null? (cdr (cdr x)))
                                    (car x)
                                    (car (cdr (cdr x)))))
                              bindings)))))))))

    (define-transformer when
      (lambda (form env)
        (let ((test (car (cdr form)))
              (body (cdr (cdr form))))
          `(,(the 'if) ,test
            (,(the 'begin) ,@body)
            #undefined))))

    (define-transformer unless
      (lambda (form env)
        (let ((test (car (cdr form)))
              (body (cdr (cdr form))))
          `(,(the 'if) ,test
            #undefined
            (,(the 'begin) ,@body)))))

    (define-transformer case
      (lambda (form env)
        (let ((key     (car (cdr form)))
              (clauses (cdr (cdr form))))
          (let ((the-key (make-identifier 'key here)))
            `(,(the 'let) ((,the-key ,key))
              ,(let loop ((clauses clauses))
                 (if (null? clauses)
                     #undefined
                     (let ((clause (car clauses)))
                       `(,(the 'if)
                         ,(if (and (identifier? (car clause))
                                   (identifier=? (the 'else)
                                                 (make-identifier (car clause) env)))
                              #t
                              `(,(the 'or)
                                ,@(map (lambda (x)
                                         `(,(the 'eqv?) ,the-key (,(the 'quote) ,x)))
                                       (car clause))))
                         ,(if (and (identifier? (cadr clause))
                                   (identifier=? (the '=>)
                                                 (make-identifier (cadr clause) env)))
                              `(,(car (cdr (cdr clause))) ,the-key)
                              `(,(the 'begin) ,@(cdr clause)))
                         ,(loop (cdr clauses)))))))))))

    (define-transformer parameterize
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          (if (null? formal)
              `(,(the 'begin) ,@body)
              (let ((bind (car formal)))
                `(,(the 'dynamic-bind) ,(car bind) ,(cadr bind)
                  (,(the 'lambda) () (,(the 'parameterize) ,(cdr formal) ,@body))))))))

    (define-transformer syntax-quote
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

    (define-transformer syntax-quasiquote
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
        (let ((ephemeron1 (make-ephemeron))
              (ephemeron2 (make-ephemeron)))
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

    (define-transformer define-syntax
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          (if (pair? formal)
              `(,(the 'define-syntax) ,(car formal) (,(the 'lambda) ,(cdr formal) ,@body))
              `(,(the 'define-macro) ,formal (,transformer (,(the 'begin) ,@body)))))))

    (define-transformer letrec-syntax
      (lambda (form env)
        (let ((formal (car (cdr form)))
              (body   (cdr (cdr form))))
          `(let ()
             ,@(map (lambda (x)
                      `(,(the 'define-syntax) ,(car x) ,(cadr x)))
                    formal)
             ,@body))))

    (define-transformer let-syntax
      (lambda (form env)
        `(,(the 'letrec-syntax) ,@(cdr form))))

    (define (auxiliary-syntax name)
      (lambda (form env)
        (let ((message
               (string-append "invalid use of auxiliary syntax: " (symbol->string name))))
          `(,(the 'error) ,message))))

    (define-macro define-auxiliary-syntax-transformer
      (lambda (form _)
        `(define-transformer ,(cadr form)
           (auxiliary-syntax ',(cadr form)))))

    (define-auxiliary-syntax-transformer else)
    (define-auxiliary-syntax-transformer =>)
    (define-auxiliary-syntax-transformer unquote)
    (define-auxiliary-syntax-transformer unquote-splicing)
    (define-auxiliary-syntax-transformer syntax-unquote)
    (define-auxiliary-syntax-transformer syntax-unquote-splicing)

    ;; library primitives

    (define (mangle name)
      (define (->string n)
        (if (symbol? n)
            (symbol->string n)
            (number->string n)))
      (define (join strs delim)
        (let loop ((res (car strs)) (strs (cdr strs)))
          (if (null? strs)
              res
              (loop (string-append res delim (car strs)) (cdr strs)))))
      (join (map ->string name) "."))

    (define-transformer define-library
      (lambda (form _)
        (let ((lib (mangle (cadr form)))
              (body (cddr form)))
          (or (find-library lib) (make-library lib))
          (for-each (lambda (expr) (eval expr lib)) body))))

    (define-transformer cond-expand
      (lambda (form _)
        (letrec
            ((test (lambda (form)
                     (or
                      (eq? form 'else)
                      (and (symbol? form)
                           (memq form (features)))
                      (and (pair? form)
                           (case (car form)
                             ((library) (find-library (mangle (cadr form))))
                             ((not) (not (test (cadr form))))
                             ((and) (let loop ((form (cdr form)))
                                      (or (null? form)
                                          (and (test (car form)) (loop (cdr form))))))
                             ((or) (let loop ((form (cdr form)))
                                     (and (pair? form)
                                          (or (test (car form)) (loop (cdr form))))))
                             (else #f)))))))
          (let loop ((clauses (cdr form)))
            (if (null? clauses)
                #undefined
                (if (test (caar clauses))
                    `(,(the 'begin) ,@(cdar clauses))
                    (loop (cdr clauses))))))))

    (define-transformer import
      (lambda (form _)
        (let ((caddr
               (lambda (x) (car (cdr (cdr x)))))
              (prefix
               (lambda (prefix symbol)
                 (string->symbol
                  (string-append
                   (symbol->string prefix)
                   (symbol->string symbol)))))
              (getlib
               (lambda (name)
                 (let ((lib (mangle name)))
                   (if (find-library lib)
                       lib
                       (error "library not found" name))))))
          (letrec
              ((extract
                (lambda (spec)
                  (case (car spec)
                    ((only rename prefix except)
                     (extract (cadr spec)))
                    (else
                     (getlib spec)))))
               (collect
                (lambda (spec)
                  (case (car spec)
                    ((only)
                     (let ((alist (collect (cadr spec))))
                       (map (lambda (var) (assq var alist)) (cddr spec))))
                    ((rename)
                     (let ((alist (collect (cadr spec)))
                           (renames (map (lambda (x) `((car x) . (cadr x))) (cddr spec))))
                       (map (lambda (s) (or (assq (car s) renames) s)) alist)))
                    ((prefix)
                     (let ((alist (collect (cadr spec))))
                       (map (lambda (s) (cons (prefix (caddr spec) (car s)) (cdr s))) alist)))
                    ((except)
                     (let ((alist (collect (cadr spec))))
                       (let loop ((alist alist))
                         (if (null? alist)
                             '()
                             (if (memq (caar alist) (cddr spec))
                                 (loop (cdr alist))
                                 (cons (car alist) (loop (cdr alist))))))))
                    (else
                     (map (lambda (x) (cons x x)) (library-exports (getlib spec))))))))
            (letrec
                ((import
                   (lambda (spec)
                     (let ((lib (extract spec))
                           (alist (collect spec)))
                       (for-each
                        (lambda (slot)
                          (library-import lib (cdr slot) (car slot)))
                        alist)))))
              (for-each import (cdr form)))))))

    (define-transformer export
      (lambda (form _)
        (letrec
            ((collect
              (lambda (spec)
                (cond
                 ((symbol? spec)
                  `(,spec . ,spec))
                 ((and (list? spec) (= (length spec) 3) (eq? (car spec) 'rename))
                  `(,(list-ref spec 1) . ,(list-ref spec 2)))
                 (else
                  (error "malformed export")))))
             (export
               (lambda (spec)
                 (let ((slot (collect spec)))
                   (library-export (car slot) (cdr slot))))))
          (for-each export (cdr form)))))

    transformers))
