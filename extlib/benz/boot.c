#if 0

=pod
/*
=cut

use strict;

my $src = <<'EOL';

(define-macro call-with-current-environment
  (lambda (form env)
    (list (cadr form) env)))

(define here
  (call-with-current-environment
   (lambda (env)
     env)))

(define (the var)                     ; synonym for #'var
  (make-identifier var here))

(define the-define (the 'define))
(define the-lambda (the 'lambda))
(define the-begin (the 'begin))
(define the-quote (the 'quote))
(define the-set! (the 'set!))
(define the-if (the 'if))
(define the-define-macro (the 'define-macro))

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

(export let let* letrec letrec*
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

(export define-library
        cond-expand
        import)

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
"\n(define-macro call-with-current-environment\n  (lambda (form env)\n    (list (cad",
"r form) env)))\n\n(define here\n  (call-with-current-environment\n   (lambda (env)\n ",
"    env)))\n\n(define (the var)                     ; synonym for #'var\n  (make-id",
"entifier var here))\n\n(define the-define (the 'define))\n(define the-lambda (the '",
"lambda))\n(define the-begin (the 'begin))\n(define the-quote (the 'quote))\n(define",
" the-set! (the 'set!))\n(define the-if (the 'if))\n(define the-define-macro (the '",
"define-macro))\n\n(define-macro syntax-error\n  (lambda (form _)\n    (apply error (",
"cdr form))))\n\n(define-macro define-auxiliary-syntax\n  (lambda (form _)\n    (defi",
"ne message\n      (string-append\n       \"invalid use of auxiliary syntax: '\" (sym",
"bol->string (cadr form)) \"'\"))\n    (list\n     the-define-macro\n     (cadr form)\n",
"     (list the-lambda '_\n           (list (the 'error) message)))))\n\n(define-aux",
"iliary-syntax else)\n(define-auxiliary-syntax =>)\n(define-auxiliary-syntax unquot",
"e)\n(define-auxiliary-syntax unquote-splicing)\n(define-auxiliary-syntax syntax-un",
"quote)\n(define-auxiliary-syntax syntax-unquote-splicing)\n\n(define-macro let\n  (l",
"ambda (form env)\n    (if (variable? (cadr form))\n        (list\n         (list th",
"e-lambda '()\n               (list the-define (cadr form)\n                     (c",
"ons the-lambda\n                           (cons (map car (car (cddr form)))\n    ",
"                             (cdr (cddr form)))))\n               (cons (cadr for",
"m) (map cadr (car (cddr form))))))\n        (cons\n         (cons\n          the-la",
"mbda\n          (cons (map car (cadr form))\n                (cddr form)))\n       ",
"  (map cadr (cadr form))))))\n\n(define-macro and\n  (lambda (form env)\n    (if (nu",
"ll? (cdr form))\n        #t\n        (if (null? (cddr form))\n            (cadr for",
"m)\n            (list the-if\n                  (cadr form)\n                  (con",
"s (the 'and) (cddr form))\n                  #f)))))\n\n(define-macro or\n  (lambda ",
"(form env)\n    (if (null? (cdr form))\n        #f\n        (let ((tmp (make-identi",
"fier 'it env)))\n          (list (the 'let)\n                (list (list tmp (cadr",
" form)))\n                (list the-if\n                      tmp\n                ",
"      tmp\n                      (cons (the 'or) (cddr form))))))))\n\n(define-macr",
"o cond\n  (lambda (form env)\n    (let ((clauses (cdr form)))\n      (if (null? cla",
"uses)\n          #undefined\n          (let ((clause (car clauses)))\n            (",
"if (and (variable? (car clause))\n                     (variable=? (the 'else) (m",
"ake-identifier (car clause) env)))\n                (cons the-begin (cdr clause))",
"\n                (if (and (variable? (cadr clause))\n                         (va",
"riable=? (the '=>) (make-identifier (cadr clause) env)))\n                    (le",
"t ((tmp (make-identifier 'tmp here)))\n                      (list (the 'let) (li",
"st (list tmp (car clause)))\n                            (list the-if tmp\n       ",
"                           (list (car (cddr clause)) tmp)\n                      ",
"            (cons (the 'cond) (cdr clauses)))))\n                    (list the-if",
" (car clause)\n                          (cons the-begin (cdr clause))\n          ",
"                (cons (the 'cond) (cdr clauses))))))))))\n\n(define-macro quasiquo",
"te\n  (lambda (form env)\n\n    (define (quasiquote? form)\n      (and (pair? form)\n",
"           (variable? (car form))\n           (variable=? (the 'quasiquote) (make",
"-identifier (car form) env))))\n\n    (define (unquote? form)\n      (and (pair? fo",
"rm)\n           (variable? (car form))\n           (variable=? (the 'unquote) (mak",
"e-identifier (car form) env))))\n\n    (define (unquote-splicing? form)\n      (and",
" (pair? form)\n           (pair? (car form))\n           (variable? (caar form))\n ",
"          (variable=? (the 'unquote-splicing) (make-identifier (caar form) env))",
"))\n\n    (define (qq depth expr)\n      (cond\n       ;; unquote\n       ((unquote? ",
"expr)\n        (if (= depth 1)\n            (car (cdr expr))\n            (list (th",
"e 'list)\n                  (list (the 'quote) (the 'unquote))\n                  ",
"(qq (- depth 1) (car (cdr expr))))))\n       ;; unquote-splicing\n       ((unquote",
"-splicing? expr)\n        (if (= depth 1)\n            (list (the 'append)\n       ",
"           (car (cdr (car expr)))\n                  (qq depth (cdr expr)))\n     ",
"       (list (the 'cons)\n                  (list (the 'list)\n                   ",
"     (list (the 'quote) (the 'unquote-splicing))\n                        (qq (- ",
"depth 1) (car (cdr (car expr)))))\n                  (qq depth (cdr expr)))))\n   ",
"    ;; quasiquote\n       ((quasiquote? expr)\n        (list (the 'list)\n         ",
"     (list (the 'quote) (the 'quasiquote))\n              (qq (+ depth 1) (car (c",
"dr expr)))))\n       ;; list\n       ((pair? expr)\n        (list (the 'cons)\n     ",
"         (qq depth (car expr))\n              (qq depth (cdr expr))))\n       ;; v",
"ector\n       ((vector? expr)\n        (list (the 'list->vector) (qq depth (vector",
"->list expr))))\n       ;; simple datum\n       (else\n        (list (the 'quote) e",
"xpr))))\n\n    (let ((x (cadr form)))\n      (qq 1 x))))\n\n(define-macro let*\n  (lam",
"bda (form env)\n    (let ((bindings (car (cdr form)))\n          (body     (cdr (c",
"dr form))))\n      (if (null? bindings)\n          `(,(the 'let) () ,@body)\n      ",
"    `(,(the 'let) ((,(car (car bindings)) ,@(cdr (car bindings))))\n            (",
",(the 'let*) (,@(cdr bindings))\n             ,@body))))))\n\n(define-macro letrec\n",
"  (lambda (form env)\n    `(,(the 'letrec*) ,@(cdr form))))\n\n(define-macro letrec",
"*\n  (lambda (form env)\n    (let ((bindings (car (cdr form)))\n          (body    ",
" (cdr (cdr form))))\n      (let ((variables (map (lambda (v) `(,v #f)) (map car b",
"indings)))\n            (initials  (map (lambda (v) `(,(the 'set!) ,@v)) bindings",
")))\n        `(,(the 'let) (,@variables)\n          ,@initials\n          ,@body)))",
"))\n\n(define-macro let-values\n  (lambda (form env)\n    `(,(the 'let*-values) ,@(c",
"dr form))))\n\n(define-macro let*-values\n  (lambda (form env)\n    (let ((formal (c",
"ar (cdr form)))\n          (body   (cdr (cdr form))))\n      (if (null? formal)\n  ",
"        `(,(the 'let) () ,@body)\n          `(,(the 'call-with-values) (,the-lamb",
"da () ,@(cdr (car formal)))\n            (,(the 'lambda) (,@(car (car formal)))\n ",
"            (,(the 'let*-values) (,@(cdr formal))\n              ,@body)))))))\n\n(",
"define-macro define-values\n  (lambda (form env)\n    (let ((formal (car (cdr form",
")))\n          (body   (cdr (cdr form))))\n      (let ((arguments (make-identifier",
" 'arguments here)))\n        `(,the-begin\n          ,@(let loop ((formal formal))",
"\n              (if (pair? formal)\n                  `((,the-define ,(car formal)",
" #undefined) ,@(loop (cdr formal)))\n                  (if (variable? formal)\n   ",
"                   `((,the-define ,formal #undefined))\n                      '()",
")))\n          (,(the 'call-with-values) (,the-lambda () ,@body)\n           (,the",
"-lambda\n            ,arguments\n            ,@(let loop ((formal formal) (args ar",
"guments))\n                (if (pair? formal)\n                    `((,the-set! ,(",
"car formal) (,(the 'car) ,args)) ,@(loop (cdr formal) `(,(the 'cdr) ,args)))\n   ",
"                 (if (variable? formal)\n                        `((,the-set! ,fo",
"rmal ,args))\n                        '()))))))))))\n\n(define-macro do\n  (lambda (",
"form env)\n    (let ((bindings (car (cdr form)))\n          (test     (car (car (c",
"dr (cdr form)))))\n          (cleanup  (cdr (car (cdr (cdr form)))))\n          (b",
"ody     (cdr (cdr (cdr form)))))\n      (let ((loop (make-identifier 'loop here))",
")\n        `(,(the 'let) ,loop ,(map (lambda (x) `(,(car x) ,(cadr x))) bindings)",
"\n          (,the-if ,test\n                   (,the-begin\n                    ,@c",
"leanup)\n                   (,the-begin\n                    ,@body\n              ",
"      (,loop ,@(map (lambda (x) (if (null? (cdr (cdr x))) (car x) (car (cdr (cdr",
" x))))) bindings)))))))))\n\n(define-macro when\n  (lambda (form env)\n    (let ((te",
"st (car (cdr form)))\n          (body (cdr (cdr form))))\n      `(,the-if ,test\n  ",
"              (,the-begin ,@body)\n                #undefined))))\n\n(define-macro ",
"unless\n  (lambda (form env)\n    (let ((test (car (cdr form)))\n          (body (c",
"dr (cdr form))))\n      `(,the-if ,test\n                #undefined\n              ",
"  (,the-begin ,@body)))))\n\n(define-macro case\n  (lambda (form env)\n    (let ((ke",
"y     (car (cdr form)))\n          (clauses (cdr (cdr form))))\n      (let ((the-k",
"ey (make-identifier 'key here)))\n        `(,(the 'let) ((,the-key ,key))\n       ",
"   ,(let loop ((clauses clauses))\n             (if (null? clauses)\n             ",
"    #undefined\n                 (let ((clause (car clauses)))\n                  ",
" `(,the-if ,(if (and (variable? (car clause))\n                                  ",
"     (variable=? (the 'else) (make-identifier (car clause) env)))\n              ",
"                    #t\n                                  `(,(the 'or) ,@(map (la",
"mbda (x) `(,(the 'eqv?) ,the-key (,the-quote ,x))) (car clause))))\n             ",
"                ,(if (and (variable? (cadr clause))\n                            ",
"           (variable=? (the '=>) (make-identifier (cadr clause) env)))\n         ",
"                         `(,(car (cdr (cdr clause))) ,the-key)\n                 ",
"                 `(,the-begin ,@(cdr clause)))\n                             ,(lo",
"op (cdr clauses)))))))))))\n\n(define-macro parameterize\n  (lambda (form env)\n    ",
"(let ((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      `(,(t",
"he 'with-parameter)\n        (,(the 'lambda) ()\n         ,@formal\n         ,@body",
")))))\n\n(define-macro syntax-quote\n  (lambda (form env)\n    (let ((renames '()))\n",
"      (letrec\n          ((rename (lambda (var)\n                     (let ((x (as",
"sq var renames)))\n                       (if x\n                           (cadr ",
"x)\n                           (begin\n                             (set! renames ",
"`((,var ,(make-identifier var env) (,(the 'make-identifier) ',var ',env)) . ,ren",
"ames))\n                             (rename var))))))\n           (walk (lambda (",
"f form)\n                   (cond\n                    ((variable? form)\n         ",
"            (f form))\n                    ((pair? form)\n                     `(,",
"(the 'cons) (walk f (car form)) (walk f (cdr form))))\n                    ((vect",
"or? form)\n                     `(,(the 'list->vector) (walk f (vector->list form",
"))))\n                    (else\n                     `(,(the 'quote) ,form))))))\n",
"        (let ((form (walk rename (cadr form))))\n          `(,(the 'let)\n        ",
"    ,(map cdr renames)\n            ,form))))))\n\n(define-macro syntax-quasiquote\n",
"  (lambda (form env)\n    (let ((renames '()))\n      (letrec\n          ((rename (",
"lambda (var)\n                     (let ((x (assq var renames)))\n                ",
"       (if x\n                           (cadr x)\n                           (beg",
"in\n                             (set! renames `((,var ,(make-identifier var env)",
" (,(the 'make-identifier) ',var ',env)) . ,renames))\n                           ",
"  (rename var)))))))\n\n        (define (syntax-quasiquote? form)\n          (and (",
"pair? form)\n               (variable? (car form))\n               (variable=? (th",
"e 'syntax-quasiquote) (make-identifier (car form) env))))\n\n        (define (synt",
"ax-unquote? form)\n          (and (pair? form)\n               (variable? (car for",
"m))\n               (variable=? (the 'syntax-unquote) (make-identifier (car form)",
" env))))\n\n        (define (syntax-unquote-splicing? form)\n          (and (pair? ",
"form)\n               (pair? (car form))\n               (variable? (caar form))\n ",
"              (variable=? (the 'syntax-unquote-splicing) (make-identifier (caar ",
"form) env))))\n\n        (define (qq depth expr)\n          (cond\n           ;; syn",
"tax-unquote\n           ((syntax-unquote? expr)\n            (if (= depth 1)\n     ",
"           (car (cdr expr))\n                (list (the 'list)\n                  ",
"    (list (the 'quote) (the 'syntax-unquote))\n                      (qq (- depth",
" 1) (car (cdr expr))))))\n           ;; syntax-unquote-splicing\n           ((synt",
"ax-unquote-splicing? expr)\n            (if (= depth 1)\n                (list (th",
"e 'append)\n                      (car (cdr (car expr)))\n                      (q",
"q depth (cdr expr)))\n                (list (the 'cons)\n                      (li",
"st (the 'list)\n                            (list (the 'quote) (the 'syntax-unquo",
"te-splicing))\n                            (qq (- depth 1) (car (cdr (car expr)))",
"))\n                      (qq depth (cdr expr)))))\n           ;; syntax-quasiquot",
"e\n           ((syntax-quasiquote? expr)\n            (list (the 'list)\n          ",
"        (list (the 'quote) (the 'quasiquote))\n                  (qq (+ depth 1) ",
"(car (cdr expr)))))\n           ;; list\n           ((pair? expr)\n            (lis",
"t (the 'cons)\n                  (qq depth (car expr))\n                  (qq dept",
"h (cdr expr))))\n           ;; vector\n           ((vector? expr)\n            (lis",
"t (the 'list->vector) (qq depth (vector->list expr))))\n           ;; variable\n  ",
"         ((variable? expr)\n            (rename expr))\n           ;; simple datum",
"\n           (else\n            (list (the 'quote) expr))))\n\n        (let ((body (",
"qq 1 (cadr form))))\n          `(,(the 'let)\n            ,(map cdr renames)\n     ",
"       ,body))))))\n\n(define (transformer f)\n  (lambda (form env)\n    (let ((regi",
"ster1 (make-register))\n          (register2 (make-register)))\n      (letrec\n    ",
"      ((wrap (lambda (var1)\n                   (let ((var2 (register1 var1)))\n  ",
"                   (if (undefined? var2)\n                         (let ((var2 (m",
"ake-identifier var1 env)))\n                           (register1 var1 var2)\n    ",
"                       (register2 var2 var1)\n                           var2)\n  ",
"                       var2))))\n           (unwrap (lambda (var2)\n              ",
"       (let ((var1 (register2 var2)))\n                       (if (undefined? var",
"1)\n                           var2\n                           var1))))\n         ",
"  (walk (lambda (f form)\n                   (cond\n                    ((variable",
"? form)\n                     (f form))\n                    ((pair? form)\n       ",
"              (cons (walk f (car form)) (walk f (cdr form))))\n                  ",
"  ((vector? form)\n                     (list->vector (walk f (vector->list form)",
")))\n                    (else\n                     form)))))\n        (let ((form",
" (cdr form)))\n          (walk unwrap (apply f (walk wrap form))))))))\n\n(define-m",
"acro define-syntax\n  (lambda (form env)\n    (let ((formal (car (cdr form)))\n    ",
"      (body   (cdr (cdr form))))\n      (if (pair? formal)\n          `(,(the 'def",
"ine-syntax) ,(car formal) (,the-lambda ,(cdr formal) ,@body))\n          `(,the-d",
"efine-macro ,formal (,(the 'transformer) (,the-begin ,@body)))))))\n\n(define-macr",
"o letrec-syntax\n  (lambda (form env)\n    (let ((formal (car (cdr form)))\n       ",
"   (body   (cdr (cdr form))))\n      `(let ()\n         ,@(map (lambda (x)\n       ",
"           `(,(the 'define-syntax) ,(car x) ,(cadr x)))\n                formal)\n",
"         ,@body))))\n\n(define-macro let-syntax\n  (lambda (form env)\n    `(,(the '",
"letrec-syntax) ,@(cdr form))))\n\n(export let let* letrec letrec*\n        let-valu",
"es let*-values define-values\n        quasiquote unquote unquote-splicing\n       ",
" and or\n        cond case else =>\n        do when unless\n        parameterize\n  ",
"      define-syntax\n        syntax-quote syntax-unquote\n        syntax-quasiquot",
"e syntax-unquote-splicing\n        let-syntax letrec-syntax\n        syntax-error)",
"\n\n(define-macro define-library\n  (lambda (form _)\n    (let ((name (cadr form))\n ",
"         (body (cddr form)))\n      (let ((old-library (current-library))\n       ",
"     (new-library (or (find-library name) (make-library name))))\n        (let ((",
"env (library-environment new-library)))\n          (current-library new-library)\n",
"          (for-each (lambda (expr) (eval expr env)) body)\n          (current-lib",
"rary old-library))))))\n\n(define-macro cond-expand\n  (lambda (form _)\n    (letrec",
"\n        ((test (lambda (form)\n                 (or\n                  (eq? form ",
"'else)\n                  (and (symbol? form)\n                       (memq form (",
"features)))\n                  (and (pair? form)\n                       (case (ca",
"r form)\n                         ((library) (find-library (cadr form)))\n        ",
"                 ((not) (not (test (cadr form))))\n                         ((and",
") (let loop ((form (cdr form)))\n                                  (or (null? for",
"m)\n                                      (and (test (car form)) (loop (cdr form)",
")))))\n                         ((or) (let loop ((form (cdr form)))\n             ",
"                    (and (pair? form)\n                                      (or ",
"(test (car form)) (loop (cdr form))))))\n                         (else #f)))))))",
"\n      (let loop ((clauses (cdr form)))\n        (if (null? clauses)\n            ",
"#undefined\n            (if (test (caar clauses))\n                `(,the-begin ,@",
"(cdar clauses))\n                (loop (cdr clauses))))))))\n\n(define-macro import",
"\n  (lambda (form _)\n    (let ((caddr\n           (lambda (x) (car (cdr (cdr x))))",
")\n          (prefix\n           (lambda (prefix symbol)\n             (string->sym",
"bol\n              (string-append\n               (symbol->string prefix)\n        ",
"       (symbol->string symbol))))))\n      (letrec\n          ((extract\n          ",
"  (lambda (spec)\n              (case (car spec)\n                ((only rename pr",
"efix except)\n                 (extract (cadr spec)))\n                (else\n     ",
"            (or (find-library spec) (error \"library not found\" spec))))))\n      ",
"     (collect\n            (lambda (spec)\n              (case (car spec)\n        ",
"        ((only)\n                 (let ((alist (collect (cadr spec))))\n          ",
"         (map (lambda (var) (assq var alist)) (cddr spec))))\n                ((r",
"ename)\n                 (let ((alist (collect (cadr spec))))\n                   ",
"(map (lambda (s) (or (assq (car s) (cddr spec)) s)) alist)))\n                ((p",
"refix)\n                 (let ((alist (collect (cadr spec))))\n                   ",
"(map (lambda (s) (cons (prefix (caddr spec) (car s)) (cdr s))) alist)))\n        ",
"        ((except)\n                 (let ((alist (collect (cadr spec))))\n        ",
"           (let loop ((alist alist))\n                     (if (null? alist)\n    ",
"                     '()\n                         (if (memq (caar alist) (cddr s",
"pec))\n                             (loop (cdr alist))\n                          ",
"   (cons (car alist) (loop (cdr alist))))))))\n                (else\n            ",
"     (let ((lib (or (find-library spec) (error \"library not found\" spec))))\n    ",
"               (map (lambda (x) (cons x x)) (library-exports lib))))))))\n       ",
" (letrec\n            ((import\n               (lambda (spec)\n                 (le",
"t ((lib (extract spec))\n                       (alist (collect spec)))\n         ",
"          (for-each\n                    (lambda (slot)\n                      (li",
"brary-import lib (cdr slot) (car slot)))\n                    alist)))))\n        ",
"  (for-each import (cdr form)))))))\n\n(export define-library\n        cond-expand\n",
"        import)\n\n",
"",
""
};

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
