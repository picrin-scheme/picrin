#if 0

=pod
/*
=cut

use strict;

my $src = <<'EOL';

(define-library (picrin base)

  (define (memoize f)
    "memoize on symbols"
    (define cache (make-dictionary))
    (lambda (sym)
      (call-with-values (lambda () (dictionary-ref cache sym))
        (lambda (value exists)
          (if exists
              value
              (begin
                (define val (f sym))
                (dictionary-set! cache sym val)
                val))))))

  (define (er-macro-transformer f)
    (lambda (mac-env)
      (lambda (expr use-env)

        (define rename
          (memoize
           (lambda (sym)
             (make-identifier sym mac-env))))

        (define (compare x y)
          (if (not (symbol? x))
              #f
              (if (not (symbol? y))
                  #f
                  (identifier=? use-env x use-env y))))

        (f expr rename compare))))

  (define-syntax syntax-error
    (er-macro-transformer
     (lambda (expr rename compare)
       (apply error (cdr expr)))))

  (define-syntax define-auxiliary-syntax
    (er-macro-transformer
     (lambda (expr r c)
       (list (r 'define-syntax) (cadr expr)
             (list (r 'lambda) '_
                   (list (r 'lambda) '_
                         (list (r 'error) "invalid use of auxiliary syntax")))))))

  (define-auxiliary-syntax else)
  (define-auxiliary-syntax =>)
  (define-auxiliary-syntax unquote)
  (define-auxiliary-syntax unquote-splicing)

  (define-syntax let
    (er-macro-transformer
     (lambda (expr r compare)
       (if (symbol? (cadr expr))
           (begin
             (define name     (car (cdr expr)))
             (define bindings (car (cdr (cdr expr))))
             (define body     (cdr (cdr (cdr expr))))
             (list (r 'let) '()
                   (list (r 'define) name
                         (cons (r 'lambda) (cons (map car bindings) body)))
                   (cons name (map cadr bindings))))
           (begin
             (set! bindings (cadr expr))
             (set! body (cddr expr))
             (cons (cons (r 'lambda) (cons (map car bindings) body))
                   (map cadr bindings)))))))

  (define-syntax cond
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((clauses (cdr expr)))
         (if (null? clauses)
             #f
             (begin
               (define clause (car clauses))
               (if (compare (r 'else) (car clause))
                   (cons (r 'begin) (cdr clause))
                   (if (if (>= (length clause) 2)
                           (compare (r '=>) (list-ref clause 1))
                           #f)
                       (list (r 'let) (list (list (r 'x) (car clause)))
                             (list (r 'if) (r 'x)
                                   (list (list-ref clause 2) (r 'x))
                                   (cons (r 'cond) (cdr clauses))))
                       (list (r 'if) (car clause)
                             (cons (r 'begin) (cdr clause))
                             (cons (r 'cond) (cdr clauses)))))))))))

  (define-syntax and
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((exprs (cdr expr)))
         (cond
          ((null? exprs)
           #t)
          ((= (length exprs) 1)
           (car exprs))
          (else
           (list (r 'let) (list (list (r 'it) (car exprs)))
                 (list (r 'if) (r 'it)
                       (cons (r 'and) (cdr exprs))
                       (r 'it)))))))))

  (define-syntax or
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((exprs (cdr expr)))
         (cond
          ((null? exprs)
           #t)
          ((= (length exprs) 1)
           (car exprs))
          (else
           (list (r 'let) (list (list (r 'it) (car exprs)))
                 (list (r 'if) (r 'it)
                       (r 'it)
                       (cons (r 'or) (cdr exprs))))))))))

  (define-syntax quasiquote
    (er-macro-transformer
     (lambda (form rename compare)

       (define (quasiquote? form)
         (and (pair? form) (compare (car form) (rename 'quasiquote))))

       (define (unquote? form)
         (and (pair? form) (compare (car form) (rename 'unquote))))

       (define (unquote-splicing? form)
         (and (pair? form) (pair? (car form))
              (compare (car (car form)) (rename 'unquote-splicing))))

       (define (qq depth expr)
         (cond
          ;; unquote
          ((unquote? expr)
           (if (= depth 1)
               (car (cdr expr))
               (list (rename 'list)
                     (list (rename 'quote) (rename 'unquote))
                     (qq (- depth 1) (car (cdr expr))))))
          ;; unquote-splicing
          ((unquote-splicing? expr)
           (if (= depth 1)
               (list (rename 'append)
                     (car (cdr (car expr)))
                     (qq depth (cdr expr)))
               (list (rename 'cons)
                     (list (rename 'list)
                           (list (rename 'quote) (rename 'unquote-splicing))
                           (qq (- depth 1) (car (cdr (car expr)))))
                     (qq depth (cdr expr)))))
          ;; quasiquote
          ((quasiquote? expr)
           (list (rename 'list)
                 (list (rename 'quote) (rename 'quasiquote))
                 (qq (+ depth 1) (car (cdr expr)))))
          ;; list
          ((pair? expr)
           (list (rename 'cons)
                 (qq depth (car expr))
                 (qq depth (cdr expr))))
          ;; vector
          ((vector? expr)
           (list (rename 'list->vector) (qq depth (vector->list expr))))
          ;; simple datum
          (else
           (list (rename 'quote) expr))))

       (let ((x (cadr form)))
         (qq 1 x)))))

  (define-syntax let*
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (cadr form))
             (body (cddr form)))
         (if (null? bindings)
             `(,(r 'let) () ,@body)
             `(,(r 'let) ((,(caar bindings)
                           ,@(cdar bindings)))
               (,(r 'let*) (,@(cdr bindings))
                ,@body)))))))

  (define-syntax letrec*
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (cadr form))
             (body (cddr form)))
         (let ((vars (map (lambda (v) `(,v #f)) (map car bindings)))
               (initials (map (lambda (v) `(,(r 'set!) ,@v)) bindings)))
           `(,(r 'let) (,@vars)
             ,@initials
             ,@body))))))

  (define-syntax letrec
    (er-macro-transformer
     (lambda (form rename compare)
       `(,(rename 'letrec*) ,@(cdr form)))))

  (define-syntax let*-values
    (er-macro-transformer
     (lambda (form r c)
       (let ((formals (cadr form)))
         (if (null? formals)
             `(,(r 'let) () ,@(cddr form))
             `(,(r 'call-with-values) (,(r 'lambda) () ,@(cdar formals))
               (,(r 'lambda) (,@(caar formals))
                (,(r 'let*-values) (,@(cdr formals))
                 ,@(cddr form)))))))))

  (define-syntax let-values
    (er-macro-transformer
     (lambda (form r c)
       `(,(r 'let*-values) ,@(cdr form)))))

  (define-syntax define-values
    (er-macro-transformer
     (lambda (form r compare)
       (let ((formal (cadr form))
             (exprs  (cddr form)))
         `(,(r 'begin)
            ,@(let loop ((formal formal))
                (if (not (pair? formal))
                    (if (symbol? formal)
                        `((,(r 'define) ,formal #f))
                        '())
                    `((,(r 'define) ,(car formal) #f) . ,(loop (cdr formal)))))
            (,(r 'call-with-values) (,(r 'lambda) () ,@exprs)
              (,(r 'lambda) ,(r 'args)
                ,@(let loop ((formal formal) (args (r 'args)))
                    (if (not (pair? formal))
                        (if (symbol? formal)
                            `((,(r 'set!) ,formal ,args))
                            '())
                        `((,(r 'set!) ,(car formal) (,(r 'car) ,args))
                          ,@(loop (cdr formal) `(,(r 'cdr) ,args))))))))))))

  (define-syntax do
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (car (cdr form)))
             (finish   (car (cdr (cdr form))))
             (body     (cdr (cdr (cdr form)))))
         `(,(r 'let) ,(r 'loop) ,(map (lambda (x)
                                        (list (car x) (cadr x)))
                                      bindings)
           (,(r 'if) ,(car finish)
            (,(r 'begin) ,@(cdr finish))
            (,(r 'begin) ,@body
             (,(r 'loop) ,@(map (lambda (x)
                                  (if (null? (cddr x))
                                      (car x)
                                      (car (cddr x))))
                                bindings)))))))))

  (define-syntax when
    (er-macro-transformer
     (lambda (expr rename compare)
       (let ((test (cadr expr))
             (body (cddr expr)))
         `(,(rename 'if) ,test
              (,(rename 'begin) ,@body)
              #f)))))

  (define-syntax unless
    (er-macro-transformer
     (lambda (expr rename compare)
       (let ((test (cadr expr))
             (body (cddr expr)))
         `(,(rename 'if) ,test
              #f
              (,(rename 'begin) ,@body))))))

  (define-syntax case
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((key (cadr expr))
             (clauses (cddr expr)))
         `(,(r 'let) ((,(r 'key) ,key))
            ,(let loop ((clauses clauses))
               (if (null? clauses)
                   #f
                   (begin
                     (define clause (car clauses))
                     `(,(r 'if) ,(if (compare (r 'else) (car clause))
                                     '#t
                                     `(,(r 'or)
                                       ,@(map (lambda (x)
                                                `(,(r 'eqv?) ,(r 'key) (,(r 'quote) ,x)))
                                              (car clause))))
                       ,(if (compare (r '=>) (list-ref clause 1))
                            `(,(list-ref clause 2) ,(r 'key))
                            `(,(r 'begin) ,@(cdr clause)))
                       ,(loop (cdr clauses)))))))))))

  (define (dynamic-bind parameters values body)
    (let* ((old-bindings
            (current-dynamic-environment))
           (binding
            (map (lambda (parameter value)
                   (cons parameter (parameter value #f)))
                 parameters
                 values))
           (new-bindings
            (cons binding old-bindings)))
      (dynamic-wind
          (lambda () (current-dynamic-environment new-bindings))
          body
          (lambda () (current-dynamic-environment old-bindings)))))

  (define-syntax parameterize
    (er-macro-transformer
     (lambda (form r compare)
       (let ((formal (cadr form))
             (body (cddr form)))
         `(,(r 'dynamic-bind)
           (list ,@(map car formal))
           (list ,@(map cadr formal))
           (,(r 'lambda) () ,@body))))))

  (define-syntax letrec-syntax
    (er-macro-transformer
     (lambda (form r c)
       (let ((formal (car (cdr form)))
             (body   (cdr (cdr form))))
         `(let ()
            ,@(map (lambda (x)
                     `(,(r 'define-syntax) ,(car x) ,(cadr x)))
                   formal)
            ,@body)))))

  (define-syntax let-syntax
    (er-macro-transformer
     (lambda (form r c)
       `(,(r 'letrec-syntax) ,@(cdr form)))))

  (export let let* letrec letrec*
          let-values let*-values define-values
          quasiquote unquote unquote-splicing
          and or
          cond case else =>
          do when unless
          parameterize
          let-syntax letrec-syntax
          syntax-error))

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
const char pic_boot[] =
EOL

my @lines = split /\n/, $src;

foreach (@lines) {
  s/\\/\\\\/g;
  s/"/\\"/g;
  print "\"$_\\n\"\n";
}

=pod
*/
=cut

print <<EOL;
;

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

const char pic_boot[] =
"\n"
"(define-library (picrin base)\n"
"\n"
"  (define (memoize f)\n"
"    \"memoize on symbols\"\n"
"    (define cache (make-dictionary))\n"
"    (lambda (sym)\n"
"      (call-with-values (lambda () (dictionary-ref cache sym))\n"
"        (lambda (value exists)\n"
"          (if exists\n"
"              value\n"
"              (begin\n"
"                (define val (f sym))\n"
"                (dictionary-set! cache sym val)\n"
"                val))))))\n"
"\n"
"  (define (er-macro-transformer f)\n"
"    (lambda (mac-env)\n"
"      (lambda (expr use-env)\n"
"\n"
"        (define rename\n"
"          (memoize\n"
"           (lambda (sym)\n"
"             (make-identifier sym mac-env))))\n"
"\n"
"        (define (compare x y)\n"
"          (if (not (symbol? x))\n"
"              #f\n"
"              (if (not (symbol? y))\n"
"                  #f\n"
"                  (identifier=? use-env x use-env y))))\n"
"\n"
"        (f expr rename compare))))\n"
"\n"
"  (define-syntax syntax-error\n"
"    (er-macro-transformer\n"
"     (lambda (expr rename compare)\n"
"       (apply error (cdr expr)))))\n"
"\n"
"  (define-syntax define-auxiliary-syntax\n"
"    (er-macro-transformer\n"
"     (lambda (expr r c)\n"
"       (list (r 'define-syntax) (cadr expr)\n"
"             (list (r 'lambda) '_\n"
"                   (list (r 'lambda) '_\n"
"                         (list (r 'error) \"invalid use of auxiliary syntax\")))))))\n"
"\n"
"  (define-auxiliary-syntax else)\n"
"  (define-auxiliary-syntax =>)\n"
"  (define-auxiliary-syntax unquote)\n"
"  (define-auxiliary-syntax unquote-splicing)\n"
"\n"
"  (define-syntax let\n"
"    (er-macro-transformer\n"
"     (lambda (expr r compare)\n"
"       (if (symbol? (cadr expr))\n"
"           (begin\n"
"             (define name     (car (cdr expr)))\n"
"             (define bindings (car (cdr (cdr expr))))\n"
"             (define body     (cdr (cdr (cdr expr))))\n"
"             (list (r 'let) '()\n"
"                   (list (r 'define) name\n"
"                         (cons (r 'lambda) (cons (map car bindings) body)))\n"
"                   (cons name (map cadr bindings))))\n"
"           (begin\n"
"             (set! bindings (cadr expr))\n"
"             (set! body (cddr expr))\n"
"             (cons (cons (r 'lambda) (cons (map car bindings) body))\n"
"                   (map cadr bindings)))))))\n"
"\n"
"  (define-syntax cond\n"
"    (er-macro-transformer\n"
"     (lambda (expr r compare)\n"
"       (let ((clauses (cdr expr)))\n"
"         (if (null? clauses)\n"
"             #f\n"
"             (begin\n"
"               (define clause (car clauses))\n"
"               (if (compare (r 'else) (car clause))\n"
"                   (cons (r 'begin) (cdr clause))\n"
"                   (if (if (>= (length clause) 2)\n"
"                           (compare (r '=>) (list-ref clause 1))\n"
"                           #f)\n"
"                       (list (r 'let) (list (list (r 'x) (car clause)))\n"
"                             (list (r 'if) (r 'x)\n"
"                                   (list (list-ref clause 2) (r 'x))\n"
"                                   (cons (r 'cond) (cdr clauses))))\n"
"                       (list (r 'if) (car clause)\n"
"                             (cons (r 'begin) (cdr clause))\n"
"                             (cons (r 'cond) (cdr clauses)))))))))))\n"
"\n"
"  (define-syntax and\n"
"    (er-macro-transformer\n"
"     (lambda (expr r compare)\n"
"       (let ((exprs (cdr expr)))\n"
"         (cond\n"
"          ((null? exprs)\n"
"           #t)\n"
"          ((= (length exprs) 1)\n"
"           (car exprs))\n"
"          (else\n"
"           (list (r 'let) (list (list (r 'it) (car exprs)))\n"
"                 (list (r 'if) (r 'it)\n"
"                       (cons (r 'and) (cdr exprs))\n"
"                       (r 'it)))))))))\n"
"\n"
"  (define-syntax or\n"
"    (er-macro-transformer\n"
"     (lambda (expr r compare)\n"
"       (let ((exprs (cdr expr)))\n"
"         (cond\n"
"          ((null? exprs)\n"
"           #t)\n"
"          ((= (length exprs) 1)\n"
"           (car exprs))\n"
"          (else\n"
"           (list (r 'let) (list (list (r 'it) (car exprs)))\n"
"                 (list (r 'if) (r 'it)\n"
"                       (r 'it)\n"
"                       (cons (r 'or) (cdr exprs))))))))))\n"
"\n"
"  (define-syntax quasiquote\n"
"    (er-macro-transformer\n"
"     (lambda (form rename compare)\n"
"\n"
"       (define (quasiquote? form)\n"
"         (and (pair? form) (compare (car form) (rename 'quasiquote))))\n"
"\n"
"       (define (unquote? form)\n"
"         (and (pair? form) (compare (car form) (rename 'unquote))))\n"
"\n"
"       (define (unquote-splicing? form)\n"
"         (and (pair? form) (pair? (car form))\n"
"              (compare (car (car form)) (rename 'unquote-splicing))))\n"
"\n"
"       (define (qq depth expr)\n"
"         (cond\n"
"          ;; unquote\n"
"          ((unquote? expr)\n"
"           (if (= depth 1)\n"
"               (car (cdr expr))\n"
"               (list (rename 'list)\n"
"                     (list (rename 'quote) (rename 'unquote))\n"
"                     (qq (- depth 1) (car (cdr expr))))))\n"
"          ;; unquote-splicing\n"
"          ((unquote-splicing? expr)\n"
"           (if (= depth 1)\n"
"               (list (rename 'append)\n"
"                     (car (cdr (car expr)))\n"
"                     (qq depth (cdr expr)))\n"
"               (list (rename 'cons)\n"
"                     (list (rename 'list)\n"
"                           (list (rename 'quote) (rename 'unquote-splicing))\n"
"                           (qq (- depth 1) (car (cdr (car expr)))))\n"
"                     (qq depth (cdr expr)))))\n"
"          ;; quasiquote\n"
"          ((quasiquote? expr)\n"
"           (list (rename 'list)\n"
"                 (list (rename 'quote) (rename 'quasiquote))\n"
"                 (qq (+ depth 1) (car (cdr expr)))))\n"
"          ;; list\n"
"          ((pair? expr)\n"
"           (list (rename 'cons)\n"
"                 (qq depth (car expr))\n"
"                 (qq depth (cdr expr))))\n"
"          ;; vector\n"
"          ((vector? expr)\n"
"           (list (rename 'list->vector) (qq depth (vector->list expr))))\n"
"          ;; simple datum\n"
"          (else\n"
"           (list (rename 'quote) expr))))\n"
"\n"
"       (let ((x (cadr form)))\n"
"         (qq 1 x)))))\n"
"\n"
"  (define-syntax let*\n"
"    (er-macro-transformer\n"
"     (lambda (form r compare)\n"
"       (let ((bindings (cadr form))\n"
"             (body (cddr form)))\n"
"         (if (null? bindings)\n"
"             `(,(r 'let) () ,@body)\n"
"             `(,(r 'let) ((,(caar bindings)\n"
"                           ,@(cdar bindings)))\n"
"               (,(r 'let*) (,@(cdr bindings))\n"
"                ,@body)))))))\n"
"\n"
"  (define-syntax letrec*\n"
"    (er-macro-transformer\n"
"     (lambda (form r compare)\n"
"       (let ((bindings (cadr form))\n"
"             (body (cddr form)))\n"
"         (let ((vars (map (lambda (v) `(,v #f)) (map car bindings)))\n"
"               (initials (map (lambda (v) `(,(r 'set!) ,@v)) bindings)))\n"
"           `(,(r 'let) (,@vars)\n"
"             ,@initials\n"
"             ,@body))))))\n"
"\n"
"  (define-syntax letrec\n"
"    (er-macro-transformer\n"
"     (lambda (form rename compare)\n"
"       `(,(rename 'letrec*) ,@(cdr form)))))\n"
"\n"
"  (define-syntax let*-values\n"
"    (er-macro-transformer\n"
"     (lambda (form r c)\n"
"       (let ((formals (cadr form)))\n"
"         (if (null? formals)\n"
"             `(,(r 'let) () ,@(cddr form))\n"
"             `(,(r 'call-with-values) (,(r 'lambda) () ,@(cdar formals))\n"
"               (,(r 'lambda) (,@(caar formals))\n"
"                (,(r 'let*-values) (,@(cdr formals))\n"
"                 ,@(cddr form)))))))))\n"
"\n"
"  (define-syntax let-values\n"
"    (er-macro-transformer\n"
"     (lambda (form r c)\n"
"       `(,(r 'let*-values) ,@(cdr form)))))\n"
"\n"
"  (define-syntax define-values\n"
"    (er-macro-transformer\n"
"     (lambda (form r compare)\n"
"       (let ((formal (cadr form))\n"
"             (exprs  (cddr form)))\n"
"         `(,(r 'begin)\n"
"            ,@(let loop ((formal formal))\n"
"                (if (not (pair? formal))\n"
"                    (if (symbol? formal)\n"
"                        `((,(r 'define) ,formal #f))\n"
"                        '())\n"
"                    `((,(r 'define) ,(car formal) #f) . ,(loop (cdr formal)))))\n"
"            (,(r 'call-with-values) (,(r 'lambda) () ,@exprs)\n"
"              (,(r 'lambda) ,(r 'args)\n"
"                ,@(let loop ((formal formal) (args (r 'args)))\n"
"                    (if (not (pair? formal))\n"
"                        (if (symbol? formal)\n"
"                            `((,(r 'set!) ,formal ,args))\n"
"                            '())\n"
"                        `((,(r 'set!) ,(car formal) (,(r 'car) ,args))\n"
"                          ,@(loop (cdr formal) `(,(r 'cdr) ,args))))))))))))\n"
"\n"
"  (define-syntax do\n"
"    (er-macro-transformer\n"
"     (lambda (form r compare)\n"
"       (let ((bindings (car (cdr form)))\n"
"             (finish   (car (cdr (cdr form))))\n"
"             (body     (cdr (cdr (cdr form)))))\n"
"         `(,(r 'let) ,(r 'loop) ,(map (lambda (x)\n"
"                                        (list (car x) (cadr x)))\n"
"                                      bindings)\n"
"           (,(r 'if) ,(car finish)\n"
"            (,(r 'begin) ,@(cdr finish))\n"
"            (,(r 'begin) ,@body\n"
"             (,(r 'loop) ,@(map (lambda (x)\n"
"                                  (if (null? (cddr x))\n"
"                                      (car x)\n"
"                                      (car (cddr x))))\n"
"                                bindings)))))))))\n"
"\n"
"  (define-syntax when\n"
"    (er-macro-transformer\n"
"     (lambda (expr rename compare)\n"
"       (let ((test (cadr expr))\n"
"             (body (cddr expr)))\n"
"         `(,(rename 'if) ,test\n"
"              (,(rename 'begin) ,@body)\n"
"              #f)))))\n"
"\n"
"  (define-syntax unless\n"
"    (er-macro-transformer\n"
"     (lambda (expr rename compare)\n"
"       (let ((test (cadr expr))\n"
"             (body (cddr expr)))\n"
"         `(,(rename 'if) ,test\n"
"              #f\n"
"              (,(rename 'begin) ,@body))))))\n"
"\n"
"  (define-syntax case\n"
"    (er-macro-transformer\n"
"     (lambda (expr r compare)\n"
"       (let ((key (cadr expr))\n"
"             (clauses (cddr expr)))\n"
"         `(,(r 'let) ((,(r 'key) ,key))\n"
"            ,(let loop ((clauses clauses))\n"
"               (if (null? clauses)\n"
"                   #f\n"
"                   (begin\n"
"                     (define clause (car clauses))\n"
"                     `(,(r 'if) ,(if (compare (r 'else) (car clause))\n"
"                                     '#t\n"
"                                     `(,(r 'or)\n"
"                                       ,@(map (lambda (x)\n"
"                                                `(,(r 'eqv?) ,(r 'key) (,(r 'quote) ,x)))\n"
"                                              (car clause))))\n"
"                       ,(if (compare (r '=>) (list-ref clause 1))\n"
"                            `(,(list-ref clause 2) ,(r 'key))\n"
"                            `(,(r 'begin) ,@(cdr clause)))\n"
"                       ,(loop (cdr clauses)))))))))))\n"
"\n"
"  (define (dynamic-bind parameters values body)\n"
"    (let* ((old-bindings\n"
"            (current-dynamic-environment))\n"
"           (binding\n"
"            (map (lambda (parameter value)\n"
"                   (cons parameter (parameter value #f)))\n"
"                 parameters\n"
"                 values))\n"
"           (new-bindings\n"
"            (cons binding old-bindings)))\n"
"      (dynamic-wind\n"
"          (lambda () (current-dynamic-environment new-bindings))\n"
"          body\n"
"          (lambda () (current-dynamic-environment old-bindings)))))\n"
"\n"
"  (define-syntax parameterize\n"
"    (er-macro-transformer\n"
"     (lambda (form r compare)\n"
"       (let ((formal (cadr form))\n"
"             (body (cddr form)))\n"
"         `(,(r 'dynamic-bind)\n"
"           (list ,@(map car formal))\n"
"           (list ,@(map cadr formal))\n"
"           (,(r 'lambda) () ,@body))))))\n"
"\n"
"  (define-syntax letrec-syntax\n"
"    (er-macro-transformer\n"
"     (lambda (form r c)\n"
"       (let ((formal (car (cdr form)))\n"
"             (body   (cdr (cdr form))))\n"
"         `(let ()\n"
"            ,@(map (lambda (x)\n"
"                     `(,(r 'define-syntax) ,(car x) ,(cadr x)))\n"
"                   formal)\n"
"            ,@body)))))\n"
"\n"
"  (define-syntax let-syntax\n"
"    (er-macro-transformer\n"
"     (lambda (form r c)\n"
"       `(,(r 'letrec-syntax) ,@(cdr form)))))\n"
"\n"
"  (export let let* letrec letrec*\n"
"          let-values let*-values define-values\n"
"          quasiquote unquote unquote-splicing\n"
"          and or\n"
"          cond case else =>\n"
"          do when unless\n"
"          parameterize\n"
"          let-syntax letrec-syntax\n"
"          syntax-error))\n"
;

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
