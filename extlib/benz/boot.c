#if 0

=pod
/*
=cut

use strict;

my $src = <<'EOL';

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
        (if (variable? (cadr form))
            (cons the-builtin-set! (cdr form))
            (error "illegal set! form" form))
        (error "illegal set! form" form))))

(builtin:define check-formal
  (builtin:lambda (formal)
    (if (null? formal)
        #t
        (if (variable? formal)
            #t
            (if (pair? formal)
                (if (variable? (car formal))
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
           (if (variable? (cadr form))
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
        (if (variable? (cadr form))
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
    (if (variable? (cadr form))
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
            (if (and (variable? (car clause))
                     (variable=? (the 'else) (make-identifier (car clause) env)))
                (cons the-begin (cdr clause))
                (if (and (variable? (cadr clause))
                         (variable=? (the '=>) (make-identifier (cadr clause) env)))
                    (let ((tmp (make-identifier 'tmp here)))
                      (list (the 'let) (list (list tmp (car clause)))
                            (list the-if tmp
                                  (list (car (cddr clause)) tmp)
                                  (cons (the 'cond) (cdr clauses)))))
                    (list the-if (car clause)
                          (cons the-begin (cdr clause))
                          (cons (the 'cond) (cdr clauses))))))))))

(define-macro quasiquote
  (lambda (form env)

    (define (quasiquote? form)
      (and (pair? form)
           (variable? (car form))
           (variable=? (the 'quasiquote) (make-identifier (car form) env))))

    (define (unquote? form)
      (and (pair? form)
           (variable? (car form))
           (variable=? (the 'unquote) (make-identifier (car form) env))))

    (define (unquote-splicing? form)
      (and (pair? form)
           (pair? (car form))
           (variable? (caar form))
           (variable=? (the 'unquote-splicing) (make-identifier (caar form) env))))

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
                  (if (variable? formal)
                      `((,the-define ,formal #undefined))
                      '())))
          (,(the 'call-with-values) (,the-lambda () ,@body)
           (,the-lambda
            ,arguments
            ,@(let loop ((formal formal) (args arguments))
                (if (pair? formal)
                    `((,the-set! ,(car formal) (,(the 'car) ,args)) ,@(loop (cdr formal) `(,(the 'cdr) ,args)))
                    (if (variable? formal)
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
                   `(,the-if ,(if (and (variable? (car clause))
                                       (variable=? (the 'else) (make-identifier (car clause) env)))
                                  #t
                                  `(,(the 'or) ,@(map (lambda (x) `(,(the 'eqv?) ,the-key (,the-quote ,x))) (car clause))))
                             ,(if (and (variable? (cadr clause))
                                       (variable=? (the '=>) (make-identifier (cadr clause) env)))
                                  `(,(car (cdr (cdr clause))) ,the-key)
                                  `(,the-begin ,@(cdr clause)))
                             ,(loop (cdr clauses)))))))))))

(define-macro parameterize
  (lambda (form env)
    (let ((formal (car (cdr form)))
          (body   (cdr (cdr form))))
      `(,(the 'with-parameter)
        (,(the 'lambda) ()
         ,@formal
         ,@body)))))

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
                    ((variable? form)
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
               (variable? (car form))
               (variable=? (the 'syntax-quasiquote) (make-identifier (car form) env))))

        (define (syntax-unquote? form)
          (and (pair? form)
               (variable? (car form))
               (variable=? (the 'syntax-unquote) (make-identifier (car form) env))))

        (define (syntax-unquote-splicing? form)
          (and (pair? form)
               (pair? (car form))
               (variable? (caar form))
               (variable=? (the 'syntax-unquote-splicing) (make-identifier (caar form) env))))

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
           ;; variable
           ((variable? expr)
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
    (let ((register1 (make-register))
          (register2 (make-register)))
      (letrec
          ((wrap (lambda (var1)
                   (let ((var2 (register1 var1)))
                     (if (undefined? var2)
                         (let ((var2 (make-identifier var1 env)))
                           (register1 var1 var2)
                           (register2 var2 var1)
                           var2)
                         var2))))
           (unwrap (lambda (var2)
                     (let ((var1 (register2 var2)))
                       (if (undefined? var1)
                           var2
                           var1))))
           (walk (lambda (f form)
                   (cond
                    ((variable? form)
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

(define-macro define-library
  (lambda (form _)
    (let ((name (cadr form))
          (body (cddr form)))
      (let ((old-library (current-library))
            (new-library (or (find-library name) (make-library name))))
        (let ((env (library-environment new-library)))
          (current-library new-library)
          (for-each (lambda (expr) (eval expr env)) body)
          (current-library old-library))))))

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
                         ((library) (find-library (cadr form)))
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
               (symbol->string symbol))))))
      (letrec
          ((extract
            (lambda (spec)
              (case (car spec)
                ((only rename prefix except)
                 (extract (cadr spec)))
                (else
                 (or (find-library spec) (error "library not found" spec))))))
           (collect
            (lambda (spec)
              (case (car spec)
                ((only)
                 (let ((alist (collect (cadr spec))))
                   (map (lambda (var) (assq var alist)) (cddr spec))))
                ((rename)
                 (let ((alist (collect (cadr spec))))
                   (map (lambda (s) (or (assq (car s) (cddr spec)) s)) alist)))
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
                 (let ((lib (or (find-library spec) (error "library not found" spec))))
                   (map (lambda (x) (cons x x)) (library-exports lib))))))))
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

(export define-library
        cond-expand
        import
        export)

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


EOL

open IN, "./boot.c";
my @data = <IN>;
close IN;

open STDOUT, ">", "./boot.c";

foreach (@data) {
  print;
  last if $_ eq "#---END---\n";
}

print "\n#endif\n\n";

print <<EOL;
const char pic_boot[][80] = {
EOL

my @lines = $src =~ /.{0,80}/gs;

foreach (@lines) {
  s/\\/\\\\/g;
  s/"/\\"/g;
  s/\n/\\n/g;
  print "\"$_\",\n";
}
print "\"\"\n";

=pod
*/
=cut

print <<EOL;
};

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
EOL

=pod

#---END---

#endif

const char pic_boot[][80] = {
"\n(builtin:define-macro call-with-current-environment\n  (builtin:lambda (form env",
")\n    (list (cadr form) env)))\n\n(builtin:define here\n  (call-with-current-enviro",
"nment\n   (builtin:lambda (env)\n     env)))\n\n(builtin:define the                 ",
"    ; synonym for #'var\n  (builtin:lambda (var)\n    (make-identifier var here)))",
"\n\n\n(builtin:define the-builtin-define (the (builtin:quote builtin:define)))\n(bui",
"ltin:define the-builtin-lambda (the (builtin:quote builtin:lambda)))\n(builtin:de",
"fine the-builtin-begin (the (builtin:quote builtin:begin)))\n(builtin:define the-",
"builtin-quote (the (builtin:quote builtin:quote)))\n(builtin:define the-builtin-s",
"et! (the (builtin:quote builtin:set!)))\n(builtin:define the-builtin-if (the (bui",
"ltin:quote builtin:if)))\n(builtin:define the-builtin-define-macro (the (builtin:",
"quote builtin:define-macro)))\n\n(builtin:define the-define (the (builtin:quote de",
"fine)))\n(builtin:define the-lambda (the (builtin:quote lambda)))\n(builtin:define",
" the-begin (the (builtin:quote begin)))\n(builtin:define the-quote (the (builtin:",
"quote quote)))\n(builtin:define the-set! (the (builtin:quote set!)))\n(builtin:def",
"ine the-if (the (builtin:quote if)))\n(builtin:define the-define-macro (the (buil",
"tin:quote define-macro)))\n\n(builtin:define-macro quote\n  (builtin:lambda (form e",
"nv)\n    (builtin:if (= (length form) 2)\n      (list the-builtin-quote (cadr form",
"))\n      (error \"illegal quote form\" form))))\n\n(builtin:define-macro if\n  (built",
"in:lambda (form env)\n    ((builtin:lambda (len)\n       (builtin:if (= len 4)\n   ",
"        (cons the-builtin-if (cdr form))\n           (builtin:if (= len 3)\n      ",
"         (list the-builtin-if (list-ref form 1) (list-ref form 2) #undefined)\n  ",
"             (error \"illegal if form\" form))))\n     (length form))))\n\n(builtin:d",
"efine-macro begin\n  (builtin:lambda (form env)\n    ((builtin:lambda (len)\n      ",
" (if (= len 1)\n           #undefined\n           (if (= len 2)\n               (ca",
"dr form)\n               (if (= len 3)\n                   (cons the-builtin-begin",
" (cdr form))\n                   (list the-builtin-begin\n                        ",
" (cadr form)\n                         (cons the-begin (cddr form)))))))\n     (le",
"ngth form))))\n\n(builtin:define-macro set!\n  (builtin:lambda (form env)\n    (if (",
"= (length form) 3)\n        (if (variable? (cadr form))\n            (cons the-bui",
"ltin-set! (cdr form))\n            (error \"illegal set! form\" form))\n        (err",
"or \"illegal set! form\" form))))\n\n(builtin:define check-formal\n  (builtin:lambda ",
"(formal)\n    (if (null? formal)\n        #t\n        (if (variable? formal)\n      ",
"      #t\n            (if (pair? formal)\n                (if (variable? (car form",
"al))\n                    (check-formal (cdr formal))\n                    #f)\n   ",
"             #f)))))\n\n(builtin:define-macro lambda\n  (builtin:lambda (form env)\n",
"    (if (= (length form) 1)\n        (error \"illegal lambda form\" form)\n        (",
"if (check-formal (cadr form))\n            (list the-builtin-lambda (cadr form) (",
"cons the-begin (cddr form)))\n            (error \"illegal lambda form\" form)))))\n",
"\n(builtin:define-macro define\n  (lambda (form env)\n    ((lambda (len)\n       (if",
" (= len 1)\n           (error \"illegal define form\" form)\n           (if (variabl",
"e? (cadr form))\n               (if (= len 3)\n                   (cons the-builti",
"n-define (cdr form))\n                   (error \"illegal define form\" form))\n    ",
"           (if (pair? (cadr form))\n                   (list the-define\n         ",
"                (car (cadr form))\n                         (cons the-lambda (con",
"s (cdr (cadr form)) (cddr form))))\n                   (error \"define: binding to",
" non-varaible object\" form)))))\n     (length form))))\n\n(builtin:define-macro def",
"ine-macro\n  (lambda (form env)\n    (if (= (length form) 3)\n        (if (variable",
"? (cadr form))\n            (cons the-builtin-define-macro (cdr form))\n          ",
"  (error \"define-macro: binding to non-variable object\" form))\n        (error \"i",
"llegal define-macro form\" form))))\n\n\n(define-macro syntax-error\n  (lambda (form ",
"_)\n    (apply error (cdr form))))\n\n(define-macro define-auxiliary-syntax\n  (lamb",
"da (form _)\n    (define message\n      (string-append\n       \"invalid use of auxi",
"liary syntax: '\" (symbol->string (cadr form)) \"'\"))\n    (list\n     the-define-ma",
"cro\n     (cadr form)\n     (list the-lambda '_\n           (list (the 'error) mess",
"age)))))\n\n(define-auxiliary-syntax else)\n(define-auxiliary-syntax =>)\n(define-au",
"xiliary-syntax unquote)\n(define-auxiliary-syntax unquote-splicing)\n(define-auxil",
"iary-syntax syntax-unquote)\n(define-auxiliary-syntax syntax-unquote-splicing)\n\n(",
"define-macro let\n  (lambda (form env)\n    (if (variable? (cadr form))\n        (l",
"ist\n         (list the-lambda '()\n               (list the-define (cadr form)\n  ",
"                   (cons the-lambda\n                           (cons (map car (c",
"ar (cddr form)))\n                                 (cdr (cddr form)))))\n         ",
"      (cons (cadr form) (map cadr (car (cddr form))))))\n        (cons\n         (",
"cons\n          the-lambda\n          (cons (map car (cadr form))\n                ",
"(cddr form)))\n         (map cadr (cadr form))))))\n\n(define-macro and\n  (lambda (",
"form env)\n    (if (null? (cdr form))\n        #t\n        (if (null? (cddr form))\n",
"            (cadr form)\n            (list the-if\n                  (cadr form)\n ",
"                 (cons (the 'and) (cddr form))\n                  #f)))))\n\n(defin",
"e-macro or\n  (lambda (form env)\n    (if (null? (cdr form))\n        #f\n        (l",
"et ((tmp (make-identifier 'it env)))\n          (list (the 'let)\n                ",
"(list (list tmp (cadr form)))\n                (list the-if\n                     ",
" tmp\n                      tmp\n                      (cons (the 'or) (cddr form)",
")))))))\n\n(define-macro cond\n  (lambda (form env)\n    (let ((clauses (cdr form)))",
"\n      (if (null? clauses)\n          #undefined\n          (let ((clause (car cla",
"uses)))\n            (if (and (variable? (car clause))\n                     (vari",
"able=? (the 'else) (make-identifier (car clause) env)))\n                (cons th",
"e-begin (cdr clause))\n                (if (and (variable? (cadr clause))\n       ",
"                  (variable=? (the '=>) (make-identifier (cadr clause) env)))\n  ",
"                  (let ((tmp (make-identifier 'tmp here)))\n                     ",
" (list (the 'let) (list (list tmp (car clause)))\n                            (li",
"st the-if tmp\n                                  (list (car (cddr clause)) tmp)\n ",
"                                 (cons (the 'cond) (cdr clauses)))))\n           ",
"         (list the-if (car clause)\n                          (cons the-begin (cd",
"r clause))\n                          (cons (the 'cond) (cdr clauses))))))))))\n\n(",
"define-macro quasiquote\n  (lambda (form env)\n\n    (define (quasiquote? form)\n   ",
"   (and (pair? form)\n           (variable? (car form))\n           (variable=? (t",
"he 'quasiquote) (make-identifier (car form) env))))\n\n    (define (unquote? form)",
"\n      (and (pair? form)\n           (variable? (car form))\n           (variable=",
"? (the 'unquote) (make-identifier (car form) env))))\n\n    (define (unquote-splic",
"ing? form)\n      (and (pair? form)\n           (pair? (car form))\n           (var",
"iable? (caar form))\n           (variable=? (the 'unquote-splicing) (make-identif",
"ier (caar form) env))))\n\n    (define (qq depth expr)\n      (cond\n       ;; unquo",
"te\n       ((unquote? expr)\n        (if (= depth 1)\n            (car (cdr expr))\n",
"            (list (the 'list)\n                  (list (the 'quote) (the 'unquote",
"))\n                  (qq (- depth 1) (car (cdr expr))))))\n       ;; unquote-spli",
"cing\n       ((unquote-splicing? expr)\n        (if (= depth 1)\n            (list ",
"(the 'append)\n                  (car (cdr (car expr)))\n                  (qq dep",
"th (cdr expr)))\n            (list (the 'cons)\n                  (list (the 'list",
")\n                        (list (the 'quote) (the 'unquote-splicing))\n          ",
"              (qq (- depth 1) (car (cdr (car expr)))))\n                  (qq dep",
"th (cdr expr)))))\n       ;; quasiquote\n       ((quasiquote? expr)\n        (list ",
"(the 'list)\n              (list (the 'quote) (the 'quasiquote))\n              (q",
"q (+ depth 1) (car (cdr expr)))))\n       ;; list\n       ((pair? expr)\n        (l",
"ist (the 'cons)\n              (qq depth (car expr))\n              (qq depth (cdr",
" expr))))\n       ;; vector\n       ((vector? expr)\n        (list (the 'list->vect",
"or) (qq depth (vector->list expr))))\n       ;; simple datum\n       (else\n       ",
" (list (the 'quote) expr))))\n\n    (let ((x (cadr form)))\n      (qq 1 x))))\n\n(def",
"ine-macro let*\n  (lambda (form env)\n    (let ((bindings (car (cdr form)))\n      ",
"    (body     (cdr (cdr form))))\n      (if (null? bindings)\n          `(,(the 'l",
"et) () ,@body)\n          `(,(the 'let) ((,(car (car bindings)) ,@(cdr (car bindi",
"ngs))))\n            (,(the 'let*) (,@(cdr bindings))\n             ,@body))))))\n\n",
"(define-macro letrec\n  (lambda (form env)\n    `(,(the 'letrec*) ,@(cdr form))))\n",
"\n(define-macro letrec*\n  (lambda (form env)\n    (let ((bindings (car (cdr form))",
")\n          (body     (cdr (cdr form))))\n      (let ((variables (map (lambda (v)",
" `(,v #f)) (map car bindings)))\n            (initials  (map (lambda (v) `(,(the ",
"'set!) ,@v)) bindings)))\n        `(,(the 'let) (,@variables)\n          ,@initial",
"s\n          ,@body)))))\n\n(define-macro let-values\n  (lambda (form env)\n    `(,(t",
"he 'let*-values) ,@(cdr form))))\n\n(define-macro let*-values\n  (lambda (form env)",
"\n    (let ((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      ",
"(if (null? formal)\n          `(,(the 'let) () ,@body)\n          `(,(the 'call-wi",
"th-values) (,the-lambda () ,@(cdr (car formal)))\n            (,(the 'lambda) (,@",
"(car (car formal)))\n             (,(the 'let*-values) (,@(cdr formal))\n         ",
"     ,@body)))))))\n\n(define-macro define-values\n  (lambda (form env)\n    (let ((",
"formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      (let ((argum",
"ents (make-identifier 'arguments here)))\n        `(,the-begin\n          ,@(let l",
"oop ((formal formal))\n              (if (pair? formal)\n                  `((,the",
"-define ,(car formal) #undefined) ,@(loop (cdr formal)))\n                  (if (",
"variable? formal)\n                      `((,the-define ,formal #undefined))\n    ",
"                  '())))\n          (,(the 'call-with-values) (,the-lambda () ,@b",
"ody)\n           (,the-lambda\n            ,arguments\n            ,@(let loop ((fo",
"rmal formal) (args arguments))\n                (if (pair? formal)\n              ",
"      `((,the-set! ,(car formal) (,(the 'car) ,args)) ,@(loop (cdr formal) `(,(t",
"he 'cdr) ,args)))\n                    (if (variable? formal)\n                   ",
"     `((,the-set! ,formal ,args))\n                        '()))))))))))\n\n(define",
"-macro do\n  (lambda (form env)\n    (let ((bindings (car (cdr form)))\n          (",
"test     (car (car (cdr (cdr form)))))\n          (cleanup  (cdr (car (cdr (cdr f",
"orm)))))\n          (body     (cdr (cdr (cdr form)))))\n      (let ((loop (make-id",
"entifier 'loop here)))\n        `(,(the 'let) ,loop ,(map (lambda (x) `(,(car x) ",
",(cadr x))) bindings)\n          (,the-if ,test\n                   (,the-begin\n  ",
"                  ,@cleanup)\n                   (,the-begin\n                    ",
",@body\n                    (,loop ,@(map (lambda (x) (if (null? (cdr (cdr x))) (",
"car x) (car (cdr (cdr x))))) bindings)))))))))\n\n(define-macro when\n  (lambda (fo",
"rm env)\n    (let ((test (car (cdr form)))\n          (body (cdr (cdr form))))\n   ",
"   `(,the-if ,test\n                (,the-begin ,@body)\n                #undefine",
"d))))\n\n(define-macro unless\n  (lambda (form env)\n    (let ((test (car (cdr form)",
"))\n          (body (cdr (cdr form))))\n      `(,the-if ,test\n                #und",
"efined\n                (,the-begin ,@body)))))\n\n(define-macro case\n  (lambda (fo",
"rm env)\n    (let ((key     (car (cdr form)))\n          (clauses (cdr (cdr form))",
"))\n      (let ((the-key (make-identifier 'key here)))\n        `(,(the 'let) ((,t",
"he-key ,key))\n          ,(let loop ((clauses clauses))\n             (if (null? c",
"lauses)\n                 #undefined\n                 (let ((clause (car clauses)",
"))\n                   `(,the-if ,(if (and (variable? (car clause))\n             ",
"                          (variable=? (the 'else) (make-identifier (car clause) ",
"env)))\n                                  #t\n                                  `(",
",(the 'or) ,@(map (lambda (x) `(,(the 'eqv?) ,the-key (,the-quote ,x))) (car cla",
"use))))\n                             ,(if (and (variable? (cadr clause))\n       ",
"                                (variable=? (the '=>) (make-identifier (cadr cla",
"use) env)))\n                                  `(,(car (cdr (cdr clause))) ,the-k",
"ey)\n                                  `(,the-begin ,@(cdr clause)))\n            ",
"                 ,(loop (cdr clauses)))))))))))\n\n(define-macro parameterize\n  (l",
"ambda (form env)\n    (let ((formal (car (cdr form)))\n          (body   (cdr (cdr",
" form))))\n      `(,(the 'with-parameter)\n        (,(the 'lambda) ()\n         ,@f",
"ormal\n         ,@body)))))\n\n(define-macro syntax-quote\n  (lambda (form env)\n    ",
"(let ((renames '()))\n      (letrec\n          ((rename (lambda (var)\n            ",
"         (let ((x (assq var renames)))\n                       (if x\n            ",
"               (cadr x)\n                           (begin\n                      ",
"       (set! renames `((,var ,(make-identifier var env) (,(the 'make-identifier)",
" ',var ',env)) . ,renames))\n                             (rename var))))))\n     ",
"      (walk (lambda (f form)\n                   (cond\n                    ((vari",
"able? form)\n                     (f form))\n                    ((pair? form)\n   ",
"                  `(,(the 'cons) (walk f (car form)) (walk f (cdr form))))\n     ",
"               ((vector? form)\n                     `(,(the 'list->vector) (walk",
" f (vector->list form))))\n                    (else\n                     `(,(the",
" 'quote) ,form))))))\n        (let ((form (walk rename (cadr form))))\n          `",
"(,(the 'let)\n            ,(map cdr renames)\n            ,form))))))\n\n(define-mac",
"ro syntax-quasiquote\n  (lambda (form env)\n    (let ((renames '()))\n      (letrec",
"\n          ((rename (lambda (var)\n                     (let ((x (assq var rename",
"s)))\n                       (if x\n                           (cadr x)\n          ",
"                 (begin\n                             (set! renames `((,var ,(mak",
"e-identifier var env) (,(the 'make-identifier) ',var ',env)) . ,renames))\n      ",
"                       (rename var)))))))\n\n        (define (syntax-quasiquote? f",
"orm)\n          (and (pair? form)\n               (variable? (car form))\n         ",
"      (variable=? (the 'syntax-quasiquote) (make-identifier (car form) env))))\n\n",
"        (define (syntax-unquote? form)\n          (and (pair? form)\n             ",
"  (variable? (car form))\n               (variable=? (the 'syntax-unquote) (make-",
"identifier (car form) env))))\n\n        (define (syntax-unquote-splicing? form)\n ",
"         (and (pair? form)\n               (pair? (car form))\n               (var",
"iable? (caar form))\n               (variable=? (the 'syntax-unquote-splicing) (m",
"ake-identifier (caar form) env))))\n\n        (define (qq depth expr)\n          (c",
"ond\n           ;; syntax-unquote\n           ((syntax-unquote? expr)\n            ",
"(if (= depth 1)\n                (car (cdr expr))\n                (list (the 'lis",
"t)\n                      (list (the 'quote) (the 'syntax-unquote))\n             ",
"         (qq (- depth 1) (car (cdr expr))))))\n           ;; syntax-unquote-splic",
"ing\n           ((syntax-unquote-splicing? expr)\n            (if (= depth 1)\n    ",
"            (list (the 'append)\n                      (car (cdr (car expr)))\n   ",
"                   (qq depth (cdr expr)))\n                (list (the 'cons)\n    ",
"                  (list (the 'list)\n                            (list (the 'quot",
"e) (the 'syntax-unquote-splicing))\n                            (qq (- depth 1) (",
"car (cdr (car expr)))))\n                      (qq depth (cdr expr)))))\n         ",
"  ;; syntax-quasiquote\n           ((syntax-quasiquote? expr)\n            (list (",
"the 'list)\n                  (list (the 'quote) (the 'quasiquote))\n             ",
"     (qq (+ depth 1) (car (cdr expr)))))\n           ;; list\n           ((pair? e",
"xpr)\n            (list (the 'cons)\n                  (qq depth (car expr))\n     ",
"             (qq depth (cdr expr))))\n           ;; vector\n           ((vector? e",
"xpr)\n            (list (the 'list->vector) (qq depth (vector->list expr))))\n    ",
"       ;; variable\n           ((variable? expr)\n            (rename expr))\n     ",
"      ;; simple datum\n           (else\n            (list (the 'quote) expr))))\n\n",
"        (let ((body (qq 1 (cadr form))))\n          `(,(the 'let)\n            ,(m",
"ap cdr renames)\n            ,body))))))\n\n(define (transformer f)\n  (lambda (form",
" env)\n    (let ((register1 (make-register))\n          (register2 (make-register)",
"))\n      (letrec\n          ((wrap (lambda (var1)\n                   (let ((var2 ",
"(register1 var1)))\n                     (if (undefined? var2)\n                  ",
"       (let ((var2 (make-identifier var1 env)))\n                           (regi",
"ster1 var1 var2)\n                           (register2 var2 var1)\n              ",
"             var2)\n                         var2))))\n           (unwrap (lambda ",
"(var2)\n                     (let ((var1 (register2 var2)))\n                     ",
"  (if (undefined? var1)\n                           var2\n                        ",
"   var1))))\n           (walk (lambda (f form)\n                   (cond\n         ",
"           ((variable? form)\n                     (f form))\n                    ",
"((pair? form)\n                     (cons (walk f (car form)) (walk f (cdr form))",
"))\n                    ((vector? form)\n                     (list->vector (walk ",
"f (vector->list form))))\n                    (else\n                     form))))",
")\n        (let ((form (cdr form)))\n          (walk unwrap (apply f (walk wrap fo",
"rm))))))))\n\n(define-macro define-syntax\n  (lambda (form env)\n    (let ((formal (",
"car (cdr form)))\n          (body   (cdr (cdr form))))\n      (if (pair? formal)\n ",
"         `(,(the 'define-syntax) ,(car formal) (,the-lambda ,(cdr formal) ,@body",
"))\n          `(,the-define-macro ,formal (,(the 'transformer) (,the-begin ,@body",
")))))))\n\n(define-macro letrec-syntax\n  (lambda (form env)\n    (let ((formal (car",
" (cdr form)))\n          (body   (cdr (cdr form))))\n      `(let ()\n         ,@(ma",
"p (lambda (x)\n                  `(,(the 'define-syntax) ,(car x) ,(cadr x)))\n   ",
"             formal)\n         ,@body))))\n\n(define-macro let-syntax\n  (lambda (fo",
"rm env)\n    `(,(the 'letrec-syntax) ,@(cdr form))))\n\n\n;;; library primitives\n\n(d",
"efine-macro define-library\n  (lambda (form _)\n    (let ((name (cadr form))\n     ",
"     (body (cddr form)))\n      (let ((old-library (current-library))\n           ",
" (new-library (or (find-library name) (make-library name))))\n        (let ((env ",
"(library-environment new-library)))\n          (current-library new-library)\n    ",
"      (for-each (lambda (expr) (eval expr env)) body)\n          (current-library",
" old-library))))))\n\n(define-macro cond-expand\n  (lambda (form _)\n    (letrec\n   ",
"     ((test (lambda (form)\n                 (or\n                  (eq? form 'els",
"e)\n                  (and (symbol? form)\n                       (memq form (feat",
"ures)))\n                  (and (pair? form)\n                       (case (car fo",
"rm)\n                         ((library) (find-library (cadr form)))\n            ",
"             ((not) (not (test (cadr form))))\n                         ((and) (l",
"et loop ((form (cdr form)))\n                                  (or (null? form)\n ",
"                                     (and (test (car form)) (loop (cdr form)))))",
")\n                         ((or) (let loop ((form (cdr form)))\n                 ",
"                (and (pair? form)\n                                      (or (tes",
"t (car form)) (loop (cdr form))))))\n                         (else #f)))))))\n   ",
"   (let loop ((clauses (cdr form)))\n        (if (null? clauses)\n            #und",
"efined\n            (if (test (caar clauses))\n                `(,the-begin ,@(cda",
"r clauses))\n                (loop (cdr clauses))))))))\n\n(define-macro import\n  (",
"lambda (form _)\n    (let ((caddr\n           (lambda (x) (car (cdr (cdr x)))))\n  ",
"        (prefix\n           (lambda (prefix symbol)\n             (string->symbol\n",
"              (string-append\n               (symbol->string prefix)\n            ",
"   (symbol->string symbol))))))\n      (letrec\n          ((extract\n            (l",
"ambda (spec)\n              (case (car spec)\n                ((only rename prefix",
" except)\n                 (extract (cadr spec)))\n                (else\n         ",
"        (or (find-library spec) (error \"library not found\" spec))))))\n          ",
" (collect\n            (lambda (spec)\n              (case (car spec)\n            ",
"    ((only)\n                 (let ((alist (collect (cadr spec))))\n              ",
"     (map (lambda (var) (assq var alist)) (cddr spec))))\n                ((renam",
"e)\n                 (let ((alist (collect (cadr spec))))\n                   (map",
" (lambda (s) (or (assq (car s) (cddr spec)) s)) alist)))\n                ((prefi",
"x)\n                 (let ((alist (collect (cadr spec))))\n                   (map",
" (lambda (s) (cons (prefix (caddr spec) (car s)) (cdr s))) alist)))\n            ",
"    ((except)\n                 (let ((alist (collect (cadr spec))))\n            ",
"       (let loop ((alist alist))\n                     (if (null? alist)\n        ",
"                 '()\n                         (if (memq (caar alist) (cddr spec)",
")\n                             (loop (cdr alist))\n                             (",
"cons (car alist) (loop (cdr alist))))))))\n                (else\n                ",
" (let ((lib (or (find-library spec) (error \"library not found\" spec))))\n        ",
"           (map (lambda (x) (cons x x)) (library-exports lib))))))))\n        (le",
"trec\n            ((import\n               (lambda (spec)\n                 (let ((",
"lib (extract spec))\n                       (alist (collect spec)))\n             ",
"      (for-each\n                    (lambda (slot)\n                      (librar",
"y-import lib (cdr slot) (car slot)))\n                    alist)))))\n          (f",
"or-each import (cdr form)))))))\n\n(define-macro export\n  (lambda (form _)\n    (le",
"trec\n        ((collect\n          (lambda (spec)\n            (cond\n             (",
"(symbol? spec)\n              `(,spec . ,spec))\n             ((and (list? spec) (",
"= (length spec) 3) (eq? (car spec) 'rename))\n              `(,(list-ref spec 1) ",
". ,(list-ref spec 2)))\n             (else\n              (error \"malformed export",
"\")))))\n         (export\n           (lambda (spec)\n             (let ((slot (coll",
"ect spec)))\n               (library-export (car slot) (cdr slot))))))\n      (for",
"-each export (cdr form)))))\n\n(export define-library\n        cond-expand\n        ",
"import\n        export)\n\n(export define lambda quote set! if begin define-macro\n ",
"       let let* letrec letrec*\n        let-values let*-values define-values\n    ",
"    quasiquote unquote unquote-splicing\n        and or\n        cond case else =>",
"\n        do when unless\n        parameterize\n        define-syntax\n        synta",
"x-quote syntax-unquote\n        syntax-quasiquote syntax-unquote-splicing\n       ",
" let-syntax letrec-syntax\n        syntax-error)\n\n\n",
"",
""
};

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
