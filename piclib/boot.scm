(builtin:define-macro call-with-current-environment
  (builtin:lambda (form env)
    (list (cadr form) env)))

(builtin:define here
  (call-with-current-environment
   (builtin:lambda (env)
     env)))

(builtin:define the                     ; synonym for #'var
  (builtin:lambda (var)
    (make-identifier var here)))


(builtin:define the-builtin-define (the (builtin:quote builtin:define)))
(builtin:define the-builtin-lambda (the (builtin:quote builtin:lambda)))
(builtin:define the-builtin-begin (the (builtin:quote builtin:begin)))
(builtin:define the-builtin-quote (the (builtin:quote builtin:quote)))
(builtin:define the-builtin-set! (the (builtin:quote builtin:set!)))
(builtin:define the-builtin-if (the (builtin:quote builtin:if)))
(builtin:define the-builtin-define-macro (the (builtin:quote builtin:define-macro)))

(builtin:define the-define (the (builtin:quote define)))
(builtin:define the-lambda (the (builtin:quote lambda)))
(builtin:define the-begin (the (builtin:quote begin)))
(builtin:define the-quote (the (builtin:quote quote)))
(builtin:define the-set! (the (builtin:quote set!)))
(builtin:define the-if (the (builtin:quote if)))
(builtin:define the-define-macro (the (builtin:quote define-macro)))

(builtin:define-macro quote
  (builtin:lambda (form env)
    (builtin:if (= (length form) 2)
      (list the-builtin-quote (cadr form))
      (error "illegal quote form" form))))

(builtin:define-macro if
  (builtin:lambda (form env)
    ((builtin:lambda (len)
       (builtin:if (= len 4)
           (cons the-builtin-if (cdr form))
           (builtin:if (= len 3)
               (list the-builtin-if (list-ref form 1) (list-ref form 2) #undefined)
               (error "illegal if form" form))))
     (length form))))

(builtin:define-macro begin
  (builtin:lambda (form env)
    ((builtin:lambda (len)
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

(builtin:define-macro set!
  (builtin:lambda (form env)
    (if (= (length form) 3)
        (if (identifier? (cadr form))
            (cons the-builtin-set! (cdr form))
            (error "illegal set! form" form))
        (error "illegal set! form" form))))

(builtin:define check-formal
  (builtin:lambda (formal)
    (if (null? formal)
        #t
        (if (identifier? formal)
            #t
            (if (pair? formal)
                (if (identifier? (car formal))
                    (check-formal (cdr formal))
                    #f)
                #f)))))

(builtin:define-macro lambda
  (builtin:lambda (form env)
    (if (= (length form) 1)
        (error "illegal lambda form" form)
        (if (check-formal (cadr form))
            (list the-builtin-lambda (cadr form) (cons the-begin (cddr form)))
            (error "illegal lambda form" form)))))

(builtin:define-macro define
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

(builtin:define-macro define-macro
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


;;; library primitives

(define (mangle name)
  (when (null? name)
    (error "library name should be a list of at least one symbols" name))

  (define (->string n)
    (cond
     ((symbol? n)
      (let ((str (symbol->string n)))
        (string-for-each
         (lambda (c)
           (when (or (char=? c #\.) (char=? c #\/))
             (error "elements of library name may not contain '.' or '/'" n)))
         str)
        str))
     ((and (number? n) (exact? n))
      (number->string n))
     (else
      (error "symbol or integer is required" n))))

  (define (join strs delim)
    (let loop ((res (car strs)) (strs (cdr strs)))
      (if (null? strs)
          res
          (loop (string-append res delim (car strs)) (cdr strs)))))

  (join (map ->string name) "."))

(define-macro define-library
  (lambda (form _)
    (let ((lib (mangle (cadr form)))
          (body (cddr form)))
      (or (find-library lib) (make-library lib))
      (for-each (lambda (expr) (eval expr lib)) body))))

(define-macro cond-expand
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
                `(,the-begin ,@(cdar clauses))
                (loop (cdr clauses))))))))

(define-macro import
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

(define-macro export
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

(export define lambda quote set! if begin define-macro
        let let* letrec letrec*
        let-values let*-values define-values
        quasiquote unquote unquote-splicing
        and or
        cond case else =>
        do when unless
        parameterize
        define-syntax
        syntax-quote syntax-unquote
        syntax-quasiquote syntax-unquote-splicing
        let-syntax letrec-syntax
        syntax-error)
