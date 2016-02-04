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


(builtin:define the-builtin-define (the (string->symbol "builtin:define")))
(builtin:define the-builtin-lambda (the (string->symbol "builtin:lambda")))
(builtin:define the-builtin-begin (the (string->symbol "builtin:begin")))
(builtin:define the-builtin-set! (the (string->symbol "builtin:set!")))
(builtin:define the-builtin-if (the (string->symbol "builtin:if")))
(builtin:define the-builtin-define-macro (the (string->symbol "builtin:define-macro")))

(builtin:define the-define (the (string->symbol "define")))
(builtin:define the-lambda (the (string->symbol "lambda")))
(builtin:define the-begin (the (string->symbol "begin")))
(builtin:define the-set! (the (string->symbol "set!")))
(builtin:define the-if (the (string->symbol "if")))
(builtin:define the-define-macro (the (string->symbol "define-macro")))

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

(define-macro quote
  (lambda (form _)
    (define the-quote (the (string->symbol "quote")))
    (define the-cons (the (string->symbol "cons")))
    (define the-vector (the (string->symbol "vector")))
    (define the-bytevector (the (string->symbol "bytevector")))
    (define the-string->symbol (the (string->symbol "string->symbol")))
    (define the-make-identifier (the (string->symbol "make-identifier")))
    (if (= (length form) 2)
        ((lambda (obj)
           (if (pair? obj)
               (list the-cons
                     (list the-quote (car obj))
                     (list the-quote (cdr obj)))
               (if (vector? obj)
                   (cons the-vector
                         (vector->list
                          (vector-map (lambda (x) (list the-quote x)) obj)))
                   (if (bytevector? obj)
                       (cons the-bytevector
                             (bytevector->list
                              (bytevector-map (lambda (x) (list the-quote x)) obj)))
                       (if (symbol? obj)
                           (list the-string->symbol (symbol->string obj))
                           (if (identifier? obj)
                               (list the-make-identifier (identifier-handle obj))
                               obj))))))
         (cadr form))
        (error "illegal quote form" form))))

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
                (if (null? (cdr clause))
                    (let ((tmp (make-identifier 'tmp here)))
                      (list (the 'let) (list (list tmp (car clause)))
                            (list the-if tmp tmp (cons (the 'cond) (cdr clauses)))))
                    (if (and (variable? (cadr clause))
                             (variable=? (the '=>) (make-identifier (cadr clause) env)))
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
                                  `(,(the 'or) ,@(map (lambda (x) `(,(the 'eqv?) ,the-key (,(the 'quote) ,x))) (car clause))))
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
                     (if var2
                         (cdr var2)
                         (let ((var2 (make-identifier var1 env)))
                           (register1 var1 var2)
                           (register2 var2 var1)
                           var2)))))
           (unwrap (lambda (var2)
                     (let ((var1 (register2 var2)))
                       (if var1
                           (cdr var1)
                           var2))))
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

(export define lambda set! if begin define-macro
        let let* letrec letrec*
        let-values let*-values define-values
        quote quasiquote unquote unquote-splicing
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
"\n\n\n(builtin:define the-builtin-define (the (string->symbol \"builtin:define\")))\n(",
"builtin:define the-builtin-lambda (the (string->symbol \"builtin:lambda\")))\n(buil",
"tin:define the-builtin-begin (the (string->symbol \"builtin:begin\")))\n(builtin:de",
"fine the-builtin-set! (the (string->symbol \"builtin:set!\")))\n(builtin:define the",
"-builtin-if (the (string->symbol \"builtin:if\")))\n(builtin:define the-builtin-def",
"ine-macro (the (string->symbol \"builtin:define-macro\")))\n\n(builtin:define the-de",
"fine (the (string->symbol \"define\")))\n(builtin:define the-lambda (the (string->s",
"ymbol \"lambda\")))\n(builtin:define the-begin (the (string->symbol \"begin\")))\n(bui",
"ltin:define the-set! (the (string->symbol \"set!\")))\n(builtin:define the-if (the ",
"(string->symbol \"if\")))\n(builtin:define the-define-macro (the (string->symbol \"d",
"efine-macro\")))\n\n(builtin:define-macro if\n  (builtin:lambda (form env)\n    ((bui",
"ltin:lambda (len)\n       (builtin:if (= len 4)\n           (cons the-builtin-if (",
"cdr form))\n           (builtin:if (= len 3)\n               (list the-builtin-if ",
"(list-ref form 1) (list-ref form 2) #undefined)\n               (error \"illegal i",
"f form\" form))))\n     (length form))))\n\n(builtin:define-macro begin\n  (builtin:l",
"ambda (form env)\n    ((builtin:lambda (len)\n       (if (= len 1)\n           #und",
"efined\n           (if (= len 2)\n               (cadr form)\n               (if (=",
" len 3)\n                   (cons the-builtin-begin (cdr form))\n                 ",
"  (list the-builtin-begin\n                         (cadr form)\n                 ",
"        (cons the-begin (cddr form)))))))\n     (length form))))\n\n(builtin:define",
"-macro set!\n  (builtin:lambda (form env)\n    (if (= (length form) 3)\n        (if",
" (variable? (cadr form))\n            (cons the-builtin-set! (cdr form))\n        ",
"    (error \"illegal set! form\" form))\n        (error \"illegal set! form\" form)))",
")\n\n(builtin:define check-formal\n  (builtin:lambda (formal)\n    (if (null? formal",
")\n        #t\n        (if (variable? formal)\n            #t\n            (if (pair",
"? formal)\n                (if (variable? (car formal))\n                    (chec",
"k-formal (cdr formal))\n                    #f)\n                #f)))))\n\n(builtin",
":define-macro lambda\n  (builtin:lambda (form env)\n    (if (= (length form) 1)\n  ",
"      (error \"illegal lambda form\" form)\n        (if (check-formal (cadr form))\n",
"            (list the-builtin-lambda (cadr form) (cons the-begin (cddr form)))\n ",
"           (error \"illegal lambda form\" form)))))\n\n(builtin:define-macro define\n",
"  (lambda (form env)\n    ((lambda (len)\n       (if (= len 1)\n           (error \"",
"illegal define form\" form)\n           (if (variable? (cadr form))\n              ",
" (if (= len 3)\n                   (cons the-builtin-define (cdr form))\n         ",
"          (error \"illegal define form\" form))\n               (if (pair? (cadr fo",
"rm))\n                   (list the-define\n                         (car (cadr for",
"m))\n                         (cons the-lambda (cons (cdr (cadr form)) (cddr form",
"))))\n                   (error \"define: binding to non-varaible object\" form))))",
")\n     (length form))))\n\n(builtin:define-macro define-macro\n  (lambda (form env)",
"\n    (if (= (length form) 3)\n        (if (variable? (cadr form))\n            (co",
"ns the-builtin-define-macro (cdr form))\n            (error \"define-macro: bindin",
"g to non-variable object\" form))\n        (error \"illegal define-macro form\" form",
"))))\n\n(define-macro quote\n  (lambda (form _)\n    (define the-quote (the (string-",
">symbol \"quote\")))\n    (define the-cons (the (string->symbol \"cons\")))\n    (defi",
"ne the-vector (the (string->symbol \"vector\")))\n    (define the-bytevector (the (",
"string->symbol \"bytevector\")))\n    (define the-string->symbol (the (string->symb",
"ol \"string->symbol\")))\n    (define the-make-identifier (the (string->symbol \"mak",
"e-identifier\")))\n    (if (= (length form) 2)\n        ((lambda (obj)\n           (",
"if (pair? obj)\n               (list the-cons\n                     (list the-quot",
"e (car obj))\n                     (list the-quote (cdr obj)))\n               (if",
" (vector? obj)\n                   (cons the-vector\n                         (vec",
"tor->list\n                          (vector-map (lambda (x) (list the-quote x)) ",
"obj)))\n                   (if (bytevector? obj)\n                       (cons the",
"-bytevector\n                             (bytevector->list\n                     ",
"         (bytevector-map (lambda (x) (list the-quote x)) obj)))\n                ",
"       (if (symbol? obj)\n                           (list the-string->symbol (sy",
"mbol->string obj))\n                           (if (identifier? obj)\n            ",
"                   (list the-make-identifier (identifier-handle obj))\n          ",
"                     obj))))))\n         (cadr form))\n        (error \"illegal quo",
"te form\" form))))\n\n(define-macro syntax-error\n  (lambda (form _)\n    (apply erro",
"r (cdr form))))\n\n(define-macro define-auxiliary-syntax\n  (lambda (form _)\n    (d",
"efine message\n      (string-append\n       \"invalid use of auxiliary syntax: '\" (",
"symbol->string (cadr form)) \"'\"))\n    (list\n     the-define-macro\n     (cadr for",
"m)\n     (list the-lambda '_\n           (list (the 'error) message)))))\n\n(define-",
"auxiliary-syntax else)\n(define-auxiliary-syntax =>)\n(define-auxiliary-syntax unq",
"uote)\n(define-auxiliary-syntax unquote-splicing)\n(define-auxiliary-syntax syntax",
"-unquote)\n(define-auxiliary-syntax syntax-unquote-splicing)\n\n(define-macro let\n ",
" (lambda (form env)\n    (if (variable? (cadr form))\n        (list\n         (list",
" the-lambda '()\n               (list the-define (cadr form)\n                    ",
" (cons the-lambda\n                           (cons (map car (car (cddr form)))\n ",
"                                (cdr (cddr form)))))\n               (cons (cadr ",
"form) (map cadr (car (cddr form))))))\n        (cons\n         (cons\n          the",
"-lambda\n          (cons (map car (cadr form))\n                (cddr form)))\n    ",
"     (map cadr (cadr form))))))\n\n(define-macro and\n  (lambda (form env)\n    (if ",
"(null? (cdr form))\n        #t\n        (if (null? (cddr form))\n            (cadr ",
"form)\n            (list the-if\n                  (cadr form)\n                  (",
"cons (the 'and) (cddr form))\n                  #f)))))\n\n(define-macro or\n  (lamb",
"da (form env)\n    (if (null? (cdr form))\n        #f\n        (let ((tmp (make-ide",
"ntifier 'it env)))\n          (list (the 'let)\n                (list (list tmp (c",
"adr form)))\n                (list the-if\n                      tmp\n             ",
"         tmp\n                      (cons (the 'or) (cddr form))))))))\n\n(define-m",
"acro cond\n  (lambda (form env)\n    (let ((clauses (cdr form)))\n      (if (null? ",
"clauses)\n          #undefined\n          (let ((clause (car clauses)))\n          ",
"  (if (and (variable? (car clause))\n                     (variable=? (the 'else)",
" (make-identifier (car clause) env)))\n                (cons the-begin (cdr claus",
"e))\n                (if (null? (cdr clause))\n                    (let ((tmp (mak",
"e-identifier 'tmp here)))\n                      (list (the 'let) (list (list tmp",
" (car clause)))\n                            (list the-if tmp tmp (cons (the 'con",
"d) (cdr clauses)))))\n                    (if (and (variable? (cadr clause))\n    ",
"                         (variable=? (the '=>) (make-identifier (cadr clause) en",
"v)))\n                        (let ((tmp (make-identifier 'tmp here)))\n          ",
"                (list (the 'let) (list (list tmp (car clause)))\n                ",
"                (list the-if tmp\n                                      (list (ca",
"r (cddr clause)) tmp)\n                                      (cons (the 'cond) (c",
"dr clauses)))))\n                        (list the-if (car clause)\n              ",
"                (cons the-begin (cdr clause))\n                              (con",
"s (the 'cond) (cdr clauses)))))))))))\n\n(define-macro quasiquote\n  (lambda (form ",
"env)\n\n    (define (quasiquote? form)\n      (and (pair? form)\n           (variabl",
"e? (car form))\n           (variable=? (the 'quasiquote) (make-identifier (car fo",
"rm) env))))\n\n    (define (unquote? form)\n      (and (pair? form)\n           (var",
"iable? (car form))\n           (variable=? (the 'unquote) (make-identifier (car f",
"orm) env))))\n\n    (define (unquote-splicing? form)\n      (and (pair? form)\n     ",
"      (pair? (car form))\n           (variable? (caar form))\n           (variable",
"=? (the 'unquote-splicing) (make-identifier (caar form) env))))\n\n    (define (qq",
" depth expr)\n      (cond\n       ;; unquote\n       ((unquote? expr)\n        (if (",
"= depth 1)\n            (car (cdr expr))\n            (list (the 'list)\n          ",
"        (list (the 'quote) (the 'unquote))\n                  (qq (- depth 1) (ca",
"r (cdr expr))))))\n       ;; unquote-splicing\n       ((unquote-splicing? expr)\n  ",
"      (if (= depth 1)\n            (list (the 'append)\n                  (car (cd",
"r (car expr)))\n                  (qq depth (cdr expr)))\n            (list (the '",
"cons)\n                  (list (the 'list)\n                        (list (the 'qu",
"ote) (the 'unquote-splicing))\n                        (qq (- depth 1) (car (cdr ",
"(car expr)))))\n                  (qq depth (cdr expr)))))\n       ;; quasiquote\n ",
"      ((quasiquote? expr)\n        (list (the 'list)\n              (list (the 'qu",
"ote) (the 'quasiquote))\n              (qq (+ depth 1) (car (cdr expr)))))\n      ",
" ;; list\n       ((pair? expr)\n        (list (the 'cons)\n              (qq depth ",
"(car expr))\n              (qq depth (cdr expr))))\n       ;; vector\n       ((vect",
"or? expr)\n        (list (the 'list->vector) (qq depth (vector->list expr))))\n   ",
"    ;; simple datum\n       (else\n        (list (the 'quote) expr))))\n\n    (let (",
"(x (cadr form)))\n      (qq 1 x))))\n\n(define-macro let*\n  (lambda (form env)\n    ",
"(let ((bindings (car (cdr form)))\n          (body     (cdr (cdr form))))\n      (",
"if (null? bindings)\n          `(,(the 'let) () ,@body)\n          `(,(the 'let) (",
"(,(car (car bindings)) ,@(cdr (car bindings))))\n            (,(the 'let*) (,@(cd",
"r bindings))\n             ,@body))))))\n\n(define-macro letrec\n  (lambda (form env",
")\n    `(,(the 'letrec*) ,@(cdr form))))\n\n(define-macro letrec*\n  (lambda (form e",
"nv)\n    (let ((bindings (car (cdr form)))\n          (body     (cdr (cdr form))))",
"\n      (let ((variables (map (lambda (v) `(,v #f)) (map car bindings)))\n        ",
"    (initials  (map (lambda (v) `(,(the 'set!) ,@v)) bindings)))\n        `(,(the",
" 'let) (,@variables)\n          ,@initials\n          ,@body)))))\n\n(define-macro l",
"et-values\n  (lambda (form env)\n    `(,(the 'let*-values) ,@(cdr form))))\n\n(defin",
"e-macro let*-values\n  (lambda (form env)\n    (let ((formal (car (cdr form)))\n   ",
"       (body   (cdr (cdr form))))\n      (if (null? formal)\n          `(,(the 'le",
"t) () ,@body)\n          `(,(the 'call-with-values) (,the-lambda () ,@(cdr (car f",
"ormal)))\n            (,(the 'lambda) (,@(car (car formal)))\n             (,(the ",
"'let*-values) (,@(cdr formal))\n              ,@body)))))))\n\n(define-macro define",
"-values\n  (lambda (form env)\n    (let ((formal (car (cdr form)))\n          (body",
"   (cdr (cdr form))))\n      (let ((arguments (make-identifier 'arguments here)))",
"\n        `(,the-begin\n          ,@(let loop ((formal formal))\n              (if ",
"(pair? formal)\n                  `((,the-define ,(car formal) #undefined) ,@(loo",
"p (cdr formal)))\n                  (if (variable? formal)\n                      ",
"`((,the-define ,formal #undefined))\n                      '())))\n          (,(th",
"e 'call-with-values) (,the-lambda () ,@body)\n           (,the-lambda\n           ",
" ,arguments\n            ,@(let loop ((formal formal) (args arguments))\n         ",
"       (if (pair? formal)\n                    `((,the-set! ,(car formal) (,(the ",
"'car) ,args)) ,@(loop (cdr formal) `(,(the 'cdr) ,args)))\n                    (i",
"f (variable? formal)\n                        `((,the-set! ,formal ,args))\n      ",
"                  '()))))))))))\n\n(define-macro do\n  (lambda (form env)\n    (let ",
"((bindings (car (cdr form)))\n          (test     (car (car (cdr (cdr form)))))\n ",
"         (cleanup  (cdr (car (cdr (cdr form)))))\n          (body     (cdr (cdr (",
"cdr form)))))\n      (let ((loop (make-identifier 'loop here)))\n        `(,(the '",
"let) ,loop ,(map (lambda (x) `(,(car x) ,(cadr x))) bindings)\n          (,the-if",
" ,test\n                   (,the-begin\n                    ,@cleanup)\n           ",
"        (,the-begin\n                    ,@body\n                    (,loop ,@(map",
" (lambda (x) (if (null? (cdr (cdr x))) (car x) (car (cdr (cdr x))))) bindings)))",
"))))))\n\n(define-macro when\n  (lambda (form env)\n    (let ((test (car (cdr form))",
")\n          (body (cdr (cdr form))))\n      `(,the-if ,test\n                (,the",
"-begin ,@body)\n                #undefined))))\n\n(define-macro unless\n  (lambda (f",
"orm env)\n    (let ((test (car (cdr form)))\n          (body (cdr (cdr form))))\n  ",
"    `(,the-if ,test\n                #undefined\n                (,the-begin ,@bod",
"y)))))\n\n(define-macro case\n  (lambda (form env)\n    (let ((key     (car (cdr for",
"m)))\n          (clauses (cdr (cdr form))))\n      (let ((the-key (make-identifier",
" 'key here)))\n        `(,(the 'let) ((,the-key ,key))\n          ,(let loop ((cla",
"uses clauses))\n             (if (null? clauses)\n                 #undefined\n    ",
"             (let ((clause (car clauses)))\n                   `(,the-if ,(if (an",
"d (variable? (car clause))\n                                       (variable=? (t",
"he 'else) (make-identifier (car clause) env)))\n                                 ",
" #t\n                                  `(,(the 'or) ,@(map (lambda (x) `(,(the 'e",
"qv?) ,the-key (,(the 'quote) ,x))) (car clause))))\n                             ",
",(if (and (variable? (cadr clause))\n                                       (vari",
"able=? (the '=>) (make-identifier (cadr clause) env)))\n                         ",
"         `(,(car (cdr (cdr clause))) ,the-key)\n                                 ",
" `(,the-begin ,@(cdr clause)))\n                             ,(loop (cdr clauses)",
"))))))))))\n\n(define-macro parameterize\n  (lambda (form env)\n    (let ((formal (c",
"ar (cdr form)))\n          (body   (cdr (cdr form))))\n      `(,(the 'with-paramet",
"er)\n        (,(the 'lambda) ()\n         ,@formal\n         ,@body)))))\n\n(define-m",
"acro syntax-quote\n  (lambda (form env)\n    (let ((renames '()))\n      (letrec\n  ",
"        ((rename (lambda (var)\n                     (let ((x (assq var renames))",
")\n                       (if x\n                           (cadr x)\n             ",
"              (begin\n                             (set! renames `((,var ,(make-i",
"dentifier var env) (,(the 'make-identifier) ',var ',env)) . ,renames))\n         ",
"                    (rename var))))))\n           (walk (lambda (f form)\n        ",
"           (cond\n                    ((variable? form)\n                     (f f",
"orm))\n                    ((pair? form)\n                     `(,(the 'cons) (wal",
"k f (car form)) (walk f (cdr form))))\n                    ((vector? form)\n      ",
"               `(,(the 'list->vector) (walk f (vector->list form))))\n           ",
"         (else\n                     `(,(the 'quote) ,form))))))\n        (let ((f",
"orm (walk rename (cadr form))))\n          `(,(the 'let)\n            ,(map cdr re",
"names)\n            ,form))))))\n\n(define-macro syntax-quasiquote\n  (lambda (form ",
"env)\n    (let ((renames '()))\n      (letrec\n          ((rename (lambda (var)\n   ",
"                  (let ((x (assq var renames)))\n                       (if x\n   ",
"                        (cadr x)\n                           (begin\n             ",
"                (set! renames `((,var ,(make-identifier var env) (,(the 'make-id",
"entifier) ',var ',env)) . ,renames))\n                             (rename var)))",
"))))\n\n        (define (syntax-quasiquote? form)\n          (and (pair? form)\n    ",
"           (variable? (car form))\n               (variable=? (the 'syntax-quasiq",
"uote) (make-identifier (car form) env))))\n\n        (define (syntax-unquote? form",
")\n          (and (pair? form)\n               (variable? (car form))\n            ",
"   (variable=? (the 'syntax-unquote) (make-identifier (car form) env))))\n\n      ",
"  (define (syntax-unquote-splicing? form)\n          (and (pair? form)\n          ",
"     (pair? (car form))\n               (variable? (caar form))\n               (v",
"ariable=? (the 'syntax-unquote-splicing) (make-identifier (caar form) env))))\n\n ",
"       (define (qq depth expr)\n          (cond\n           ;; syntax-unquote\n    ",
"       ((syntax-unquote? expr)\n            (if (= depth 1)\n                (car ",
"(cdr expr))\n                (list (the 'list)\n                      (list (the '",
"quote) (the 'syntax-unquote))\n                      (qq (- depth 1) (car (cdr ex",
"pr))))))\n           ;; syntax-unquote-splicing\n           ((syntax-unquote-splic",
"ing? expr)\n            (if (= depth 1)\n                (list (the 'append)\n     ",
"                 (car (cdr (car expr)))\n                      (qq depth (cdr exp",
"r)))\n                (list (the 'cons)\n                      (list (the 'list)\n ",
"                           (list (the 'quote) (the 'syntax-unquote-splicing))\n  ",
"                          (qq (- depth 1) (car (cdr (car expr)))))\n             ",
"         (qq depth (cdr expr)))))\n           ;; syntax-quasiquote\n           ((s",
"yntax-quasiquote? expr)\n            (list (the 'list)\n                  (list (t",
"he 'quote) (the 'quasiquote))\n                  (qq (+ depth 1) (car (cdr expr))",
")))\n           ;; list\n           ((pair? expr)\n            (list (the 'cons)\n  ",
"                (qq depth (car expr))\n                  (qq depth (cdr expr))))\n",
"           ;; vector\n           ((vector? expr)\n            (list (the 'list->ve",
"ctor) (qq depth (vector->list expr))))\n           ;; variable\n           ((varia",
"ble? expr)\n            (rename expr))\n           ;; simple datum\n           (els",
"e\n            (list (the 'quote) expr))))\n\n        (let ((body (qq 1 (cadr form)",
")))\n          `(,(the 'let)\n            ,(map cdr renames)\n            ,body))))",
"))\n\n(define (transformer f)\n  (lambda (form env)\n    (let ((register1 (make-regi",
"ster))\n          (register2 (make-register)))\n      (letrec\n          ((wrap (la",
"mbda (var1)\n                   (let ((var2 (register1 var1)))\n                  ",
"   (if var2\n                         (cdr var2)\n                         (let ((",
"var2 (make-identifier var1 env)))\n                           (register1 var1 var",
"2)\n                           (register2 var2 var1)\n                           v",
"ar2)))))\n           (unwrap (lambda (var2)\n                     (let ((var1 (reg",
"ister2 var2)))\n                       (if var1\n                           (cdr v",
"ar1)\n                           var2))))\n           (walk (lambda (f form)\n     ",
"              (cond\n                    ((variable? form)\n                     (",
"f form))\n                    ((pair? form)\n                     (cons (walk f (c",
"ar form)) (walk f (cdr form))))\n                    ((vector? form)\n            ",
"         (list->vector (walk f (vector->list form))))\n                    (else\n",
"                     form)))))\n        (let ((form (cdr form)))\n          (walk ",
"unwrap (apply f (walk wrap form))))))))\n\n(define-macro define-syntax\n  (lambda (",
"form env)\n    (let ((formal (car (cdr form)))\n          (body   (cdr (cdr form))",
"))\n      (if (pair? formal)\n          `(,(the 'define-syntax) ,(car formal) (,th",
"e-lambda ,(cdr formal) ,@body))\n          `(,the-define-macro ,formal (,(the 'tr",
"ansformer) (,the-begin ,@body)))))))\n\n(define-macro letrec-syntax\n  (lambda (for",
"m env)\n    (let ((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n",
"      `(let ()\n         ,@(map (lambda (x)\n                  `(,(the 'define-syn",
"tax) ,(car x) ,(cadr x)))\n                formal)\n         ,@body))))\n\n(define-m",
"acro let-syntax\n  (lambda (form env)\n    `(,(the 'letrec-syntax) ,@(cdr form))))",
"\n\n\n;;; library primitives\n\n(define-macro define-library\n  (lambda (form _)\n    (",
"let ((name (cadr form))\n          (body (cddr form)))\n      (let ((old-library (",
"current-library))\n            (new-library (or (find-library name) (make-library",
" name))))\n        (let ((env (library-environment new-library)))\n          (curr",
"ent-library new-library)\n          (for-each (lambda (expr) (eval expr env)) bod",
"y)\n          (current-library old-library))))))\n\n(define-macro cond-expand\n  (la",
"mbda (form _)\n    (letrec\n        ((test (lambda (form)\n                 (or\n   ",
"               (eq? form 'else)\n                  (and (symbol? form)\n          ",
"             (memq form (features)))\n                  (and (pair? form)\n       ",
"                (case (car form)\n                         ((library) (find-libra",
"ry (cadr form)))\n                         ((not) (not (test (cadr form))))\n     ",
"                    ((and) (let loop ((form (cdr form)))\n                       ",
"           (or (null? form)\n                                      (and (test (ca",
"r form)) (loop (cdr form))))))\n                         ((or) (let loop ((form (",
"cdr form)))\n                                 (and (pair? form)\n                 ",
"                     (or (test (car form)) (loop (cdr form))))))\n               ",
"          (else #f)))))))\n      (let loop ((clauses (cdr form)))\n        (if (nu",
"ll? clauses)\n            #undefined\n            (if (test (caar clauses))\n      ",
"          `(,the-begin ,@(cdar clauses))\n                (loop (cdr clauses)))))",
")))\n\n(define-macro import\n  (lambda (form _)\n    (let ((caddr\n           (lambda",
" (x) (car (cdr (cdr x)))))\n          (prefix\n           (lambda (prefix symbol)\n",
"             (string->symbol\n              (string-append\n               (symbol",
"->string prefix)\n               (symbol->string symbol))))))\n      (letrec\n     ",
"     ((extract\n            (lambda (spec)\n              (case (car spec)\n       ",
"         ((only rename prefix except)\n                 (extract (cadr spec)))\n  ",
"              (else\n                 (or (find-library spec) (error \"library not",
" found\" spec))))))\n           (collect\n            (lambda (spec)\n              ",
"(case (car spec)\n                ((only)\n                 (let ((alist (collect ",
"(cadr spec))))\n                   (map (lambda (var) (assq var alist)) (cddr spe",
"c))))\n                ((rename)\n                 (let ((alist (collect (cadr spe",
"c)))\n                       (renames (map (lambda (x) `((car x) . (cadr x))) (cd",
"dr spec))))\n                   (map (lambda (s) (or (assq (car s) renames) s)) a",
"list)))\n                ((prefix)\n                 (let ((alist (collect (cadr s",
"pec))))\n                   (map (lambda (s) (cons (prefix (caddr spec) (car s)) ",
"(cdr s))) alist)))\n                ((except)\n                 (let ((alist (coll",
"ect (cadr spec))))\n                   (let loop ((alist alist))\n                ",
"     (if (null? alist)\n                         '()\n                         (if",
" (memq (caar alist) (cddr spec))\n                             (loop (cdr alist))",
"\n                             (cons (car alist) (loop (cdr alist))))))))\n       ",
"         (else\n                 (let ((lib (or (find-library spec) (error \"libra",
"ry not found\" spec))))\n                   (map (lambda (x) (cons x x)) (library-",
"exports lib))))))))\n        (letrec\n            ((import\n               (lambda ",
"(spec)\n                 (let ((lib (extract spec))\n                       (alist",
" (collect spec)))\n                   (for-each\n                    (lambda (slot",
")\n                      (library-import lib (cdr slot) (car slot)))\n            ",
"        alist)))))\n          (for-each import (cdr form)))))))\n\n(define-macro ex",
"port\n  (lambda (form _)\n    (letrec\n        ((collect\n          (lambda (spec)\n ",
"           (cond\n             ((symbol? spec)\n              `(,spec . ,spec))\n  ",
"           ((and (list? spec) (= (length spec) 3) (eq? (car spec) 'rename))\n    ",
"          `(,(list-ref spec 1) . ,(list-ref spec 2)))\n             (else\n       ",
"       (error \"malformed export\")))))\n         (export\n           (lambda (spec)",
"\n             (let ((slot (collect spec)))\n               (library-export (car s",
"lot) (cdr slot))))))\n      (for-each export (cdr form)))))\n\n(export define lambd",
"a set! if begin define-macro\n        let let* letrec letrec*\n        let-values ",
"let*-values define-values\n        quote quasiquote unquote unquote-splicing\n    ",
"    and or\n        cond case else =>\n        do when unless\n        parameterize",
"\n        define-syntax\n        syntax-quote syntax-unquote\n        syntax-quasiq",
"uote syntax-unquote-splicing\n        let-syntax letrec-syntax\n        syntax-err",
"or)\n\n\n",
"",
""
};

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
