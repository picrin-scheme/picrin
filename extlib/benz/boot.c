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
      (if (null? formal)
          `(,the-begin ,@body)
          (let ((bind (car formal)))
            `(,(the 'dynamic-bind) ,(car bind) ,(cadr bind)
              (,the-lambda () (,(the 'parameterize) ,(cdr formal) ,@body))))))))

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
#include "picrin.h"
#include "picrin/extra.h"

static const char boot_rom[][80] = {
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

void
pic_boot(pic_state *pic)
{
  pic_load_cstr(pic, &boot_rom[0][0]);
}

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

#include "picrin.h"
#include "picrin/extra.h"

static const char boot_rom[][80] = {
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
"= (length form) 3)\n        (if (identifier? (cadr form))\n            (cons the-b",
"uiltin-set! (cdr form))\n            (error \"illegal set! form\" form))\n        (e",
"rror \"illegal set! form\" form))))\n\n(builtin:define check-formal\n  (builtin:lambd",
"a (formal)\n    (if (null? formal)\n        #t\n        (if (identifier? formal)\n  ",
"          #t\n            (if (pair? formal)\n                (if (identifier? (ca",
"r formal))\n                    (check-formal (cdr formal))\n                    #",
"f)\n                #f)))))\n\n(builtin:define-macro lambda\n  (builtin:lambda (form",
" env)\n    (if (= (length form) 1)\n        (error \"illegal lambda form\" form)\n   ",
"     (if (check-formal (cadr form))\n            (list the-builtin-lambda (cadr f",
"orm) (cons the-begin (cddr form)))\n            (error \"illegal lambda form\" form",
")))))\n\n(builtin:define-macro define\n  (lambda (form env)\n    ((lambda (len)\n    ",
"   (if (= len 1)\n           (error \"illegal define form\" form)\n           (if (i",
"dentifier? (cadr form))\n               (if (= len 3)\n                   (cons th",
"e-builtin-define (cdr form))\n                   (error \"illegal define form\" for",
"m))\n               (if (pair? (cadr form))\n                   (list the-define\n ",
"                        (car (cadr form))\n                         (cons the-lam",
"bda (cons (cdr (cadr form)) (cddr form))))\n                   (error \"define: bi",
"nding to non-varaible object\" form)))))\n     (length form))))\n\n(builtin:define-m",
"acro define-macro\n  (lambda (form env)\n    (if (= (length form) 3)\n        (if (",
"identifier? (cadr form))\n            (cons the-builtin-define-macro (cdr form))\n",
"            (error \"define-macro: binding to non-variable object\" form))\n       ",
" (error \"illegal define-macro form\" form))))\n\n\n(define-macro syntax-error\n  (lam",
"bda (form _)\n    (apply error (cdr form))))\n\n(define-macro define-auxiliary-synt",
"ax\n  (lambda (form _)\n    (define message\n      (string-append\n       \"invalid u",
"se of auxiliary syntax: '\" (symbol->string (cadr form)) \"'\"))\n    (list\n     the",
"-define-macro\n     (cadr form)\n     (list the-lambda '_\n           (list (the 'e",
"rror) message)))))\n\n(define-auxiliary-syntax else)\n(define-auxiliary-syntax =>)\n",
"(define-auxiliary-syntax unquote)\n(define-auxiliary-syntax unquote-splicing)\n(de",
"fine-auxiliary-syntax syntax-unquote)\n(define-auxiliary-syntax syntax-unquote-sp",
"licing)\n\n(define-macro let\n  (lambda (form env)\n    (if (identifier? (cadr form)",
")\n        (list\n         (list the-lambda '()\n               (list the-define (c",
"adr form)\n                     (cons the-lambda\n                           (cons",
" (map car (car (cddr form)))\n                                 (cdr (cddr form)))",
"))\n               (cons (cadr form) (map cadr (car (cddr form))))))\n        (con",
"s\n         (cons\n          the-lambda\n          (cons (map car (cadr form))\n    ",
"            (cddr form)))\n         (map cadr (cadr form))))))\n\n(define-macro and",
"\n  (lambda (form env)\n    (if (null? (cdr form))\n        #t\n        (if (null? (",
"cddr form))\n            (cadr form)\n            (list the-if\n                  (",
"cadr form)\n                  (cons (the 'and) (cddr form))\n                  #f)",
"))))\n\n(define-macro or\n  (lambda (form env)\n    (if (null? (cdr form))\n        #",
"f\n        (let ((tmp (make-identifier 'it env)))\n          (list (the 'let)\n    ",
"            (list (list tmp (cadr form)))\n                (list the-if\n         ",
"             tmp\n                      tmp\n                      (cons (the 'or)",
" (cddr form))))))))\n\n(define-macro cond\n  (lambda (form env)\n    (let ((clauses ",
"(cdr form)))\n      (if (null? clauses)\n          #undefined\n          (let ((cla",
"use (car clauses)))\n            (if (and (identifier? (car clause))\n            ",
"         (identifier=? (the 'else) (make-identifier (car clause) env)))\n        ",
"        (cons the-begin (cdr clause))\n                (if (null? (cdr clause))\n ",
"                   (let ((tmp (make-identifier 'tmp here)))\n                    ",
"  (list (the 'let) (list (list tmp (car clause)))\n                            (l",
"ist the-if tmp tmp (cons (the 'cond) (cdr clauses)))))\n                    (if (",
"and (identifier? (cadr clause))\n                             (identifier=? (the ",
"'=>) (make-identifier (cadr clause) env)))\n                        (let ((tmp (m",
"ake-identifier 'tmp here)))\n                          (list (the 'let) (list (li",
"st tmp (car clause)))\n                                (list the-if tmp\n         ",
"                             (list (car (cddr clause)) tmp)\n                    ",
"                  (cons (the 'cond) (cdr clauses)))))\n                        (l",
"ist the-if (car clause)\n                              (cons the-begin (cdr claus",
"e))\n                              (cons (the 'cond) (cdr clauses)))))))))))\n\n(de",
"fine-macro quasiquote\n  (lambda (form env)\n\n    (define (quasiquote? form)\n     ",
" (and (pair? form)\n           (identifier? (car form))\n           (identifier=? ",
"(the 'quasiquote) (make-identifier (car form) env))))\n\n    (define (unquote? for",
"m)\n      (and (pair? form)\n           (identifier? (car form))\n           (ident",
"ifier=? (the 'unquote) (make-identifier (car form) env))))\n\n    (define (unquote",
"-splicing? form)\n      (and (pair? form)\n           (pair? (car form))\n         ",
"  (identifier? (caar form))\n           (identifier=? (the 'unquote-splicing) (ma",
"ke-identifier (caar form) env))))\n\n    (define (qq depth expr)\n      (cond\n     ",
"  ;; unquote\n       ((unquote? expr)\n        (if (= depth 1)\n            (car (c",
"dr expr))\n            (list (the 'list)\n                  (list (the 'quote) (th",
"e 'unquote))\n                  (qq (- depth 1) (car (cdr expr))))))\n       ;; un",
"quote-splicing\n       ((unquote-splicing? expr)\n        (if (= depth 1)\n        ",
"    (list (the 'append)\n                  (car (cdr (car expr)))\n               ",
"   (qq depth (cdr expr)))\n            (list (the 'cons)\n                  (list ",
"(the 'list)\n                        (list (the 'quote) (the 'unquote-splicing))\n",
"                        (qq (- depth 1) (car (cdr (car expr)))))\n               ",
"   (qq depth (cdr expr)))))\n       ;; quasiquote\n       ((quasiquote? expr)\n    ",
"    (list (the 'list)\n              (list (the 'quote) (the 'quasiquote))\n      ",
"        (qq (+ depth 1) (car (cdr expr)))))\n       ;; list\n       ((pair? expr)\n",
"        (list (the 'cons)\n              (qq depth (car expr))\n              (qq ",
"depth (cdr expr))))\n       ;; vector\n       ((vector? expr)\n        (list (the '",
"list->vector) (qq depth (vector->list expr))))\n       ;; simple datum\n       (el",
"se\n        (list (the 'quote) expr))))\n\n    (let ((x (cadr form)))\n      (qq 1 x",
"))))\n\n(define-macro let*\n  (lambda (form env)\n    (let ((bindings (car (cdr form",
")))\n          (body     (cdr (cdr form))))\n      (if (null? bindings)\n          ",
"`(,(the 'let) () ,@body)\n          `(,(the 'let) ((,(car (car bindings)) ,@(cdr ",
"(car bindings))))\n            (,(the 'let*) (,@(cdr bindings))\n             ,@bo",
"dy))))))\n\n(define-macro letrec\n  (lambda (form env)\n    `(,(the 'letrec*) ,@(cdr",
" form))))\n\n(define-macro letrec*\n  (lambda (form env)\n    (let ((bindings (car (",
"cdr form)))\n          (body     (cdr (cdr form))))\n      (let ((variables (map (",
"lambda (v) `(,v #f)) (map car bindings)))\n            (initials  (map (lambda (v",
") `(,(the 'set!) ,@v)) bindings)))\n        `(,(the 'let) (,@variables)\n         ",
" ,@initials\n          ,@body)))))\n\n(define-macro let-values\n  (lambda (form env)",
"\n    `(,(the 'let*-values) ,@(cdr form))))\n\n(define-macro let*-values\n  (lambda ",
"(form env)\n    (let ((formal (car (cdr form)))\n          (body   (cdr (cdr form)",
")))\n      (if (null? formal)\n          `(,(the 'let) () ,@body)\n          `(,(th",
"e 'call-with-values) (,the-lambda () ,@(cdr (car formal)))\n            (,(the 'l",
"ambda) (,@(car (car formal)))\n             (,(the 'let*-values) (,@(cdr formal))",
"\n              ,@body)))))))\n\n(define-macro define-values\n  (lambda (form env)\n ",
"   (let ((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      (l",
"et ((arguments (make-identifier 'arguments here)))\n        `(,the-begin\n        ",
"  ,@(let loop ((formal formal))\n              (if (pair? formal)\n               ",
"   `((,the-define ,(car formal) #undefined) ,@(loop (cdr formal)))\n             ",
"     (if (identifier? formal)\n                      `((,the-define ,formal #unde",
"fined))\n                      '())))\n          (,(the 'call-with-values) (,the-l",
"ambda () ,@body)\n           (,the-lambda\n            ,arguments\n            ,@(l",
"et loop ((formal formal) (args arguments))\n                (if (pair? formal)\n  ",
"                  `((,the-set! ,(car formal) (,(the 'car) ,args)) ,@(loop (cdr f",
"ormal) `(,(the 'cdr) ,args)))\n                    (if (identifier? formal)\n     ",
"                   `((,the-set! ,formal ,args))\n                        '())))))",
")))))\n\n(define-macro do\n  (lambda (form env)\n    (let ((bindings (car (cdr form)",
"))\n          (test     (car (car (cdr (cdr form)))))\n          (cleanup  (cdr (c",
"ar (cdr (cdr form)))))\n          (body     (cdr (cdr (cdr form)))))\n      (let (",
"(loop (make-identifier 'loop here)))\n        `(,(the 'let) ,loop ,(map (lambda (",
"x) `(,(car x) ,(cadr x))) bindings)\n          (,the-if ,test\n                   ",
"(,the-begin\n                    ,@cleanup)\n                   (,the-begin\n      ",
"              ,@body\n                    (,loop ,@(map (lambda (x) (if (null? (c",
"dr (cdr x))) (car x) (car (cdr (cdr x))))) bindings)))))))))\n\n(define-macro when",
"\n  (lambda (form env)\n    (let ((test (car (cdr form)))\n          (body (cdr (cd",
"r form))))\n      `(,the-if ,test\n                (,the-begin ,@body)\n           ",
"     #undefined))))\n\n(define-macro unless\n  (lambda (form env)\n    (let ((test (",
"car (cdr form)))\n          (body (cdr (cdr form))))\n      `(,the-if ,test\n      ",
"          #undefined\n                (,the-begin ,@body)))))\n\n(define-macro case",
"\n  (lambda (form env)\n    (let ((key     (car (cdr form)))\n          (clauses (c",
"dr (cdr form))))\n      (let ((the-key (make-identifier 'key here)))\n        `(,(",
"the 'let) ((,the-key ,key))\n          ,(let loop ((clauses clauses))\n           ",
"  (if (null? clauses)\n                 #undefined\n                 (let ((clause",
" (car clauses)))\n                   `(,the-if ,(if (and (identifier? (car clause",
"))\n                                       (identifier=? (the 'else) (make-identi",
"fier (car clause) env)))\n                                  #t\n                  ",
"                `(,(the 'or) ,@(map (lambda (x) `(,(the 'eqv?) ,the-key (,the-qu",
"ote ,x))) (car clause))))\n                             ,(if (and (identifier? (c",
"adr clause))\n                                       (identifier=? (the '=>) (mak",
"e-identifier (cadr clause) env)))\n                                  `(,(car (cdr",
" (cdr clause))) ,the-key)\n                                  `(,the-begin ,@(cdr ",
"clause)))\n                             ,(loop (cdr clauses)))))))))))\n\n(define-m",
"acro parameterize\n  (lambda (form env)\n    (let ((formal (car (cdr form)))\n     ",
"     (body   (cdr (cdr form))))\n      (if (null? formal)\n          `(,the-begin ",
",@body)\n          (let ((bind (car formal)))\n            `(,(the 'dynamic-bind) ",
",(car bind) ,(cadr bind)\n              (,the-lambda () (,(the 'parameterize) ,(c",
"dr formal) ,@body))))))))\n\n(define-macro syntax-quote\n  (lambda (form env)\n    (",
"let ((renames '()))\n      (letrec\n          ((rename (lambda (var)\n             ",
"        (let ((x (assq var renames)))\n                       (if x\n             ",
"              (cadr x)\n                           (begin\n                       ",
"      (set! renames `((,var ,(make-identifier var env) (,(the 'make-identifier) ",
"',var ',env)) . ,renames))\n                             (rename var))))))\n      ",
"     (walk (lambda (f form)\n                   (cond\n                    ((ident",
"ifier? form)\n                     (f form))\n                    ((pair? form)\n  ",
"                   `(,(the 'cons) (walk f (car form)) (walk f (cdr form))))\n    ",
"                ((vector? form)\n                     `(,(the 'list->vector) (wal",
"k f (vector->list form))))\n                    (else\n                     `(,(th",
"e 'quote) ,form))))))\n        (let ((form (walk rename (cadr form))))\n          ",
"`(,(the 'let)\n            ,(map cdr renames)\n            ,form))))))\n\n(define-ma",
"cro syntax-quasiquote\n  (lambda (form env)\n    (let ((renames '()))\n      (letre",
"c\n          ((rename (lambda (var)\n                     (let ((x (assq var renam",
"es)))\n                       (if x\n                           (cadr x)\n         ",
"                  (begin\n                             (set! renames `((,var ,(ma",
"ke-identifier var env) (,(the 'make-identifier) ',var ',env)) . ,renames))\n     ",
"                        (rename var)))))))\n\n        (define (syntax-quasiquote? ",
"form)\n          (and (pair? form)\n               (identifier? (car form))\n      ",
"         (identifier=? (the 'syntax-quasiquote) (make-identifier (car form) env)",
")))\n\n        (define (syntax-unquote? form)\n          (and (pair? form)\n        ",
"       (identifier? (car form))\n               (identifier=? (the 'syntax-unquot",
"e) (make-identifier (car form) env))))\n\n        (define (syntax-unquote-splicing",
"? form)\n          (and (pair? form)\n               (pair? (car form))\n          ",
"     (identifier? (caar form))\n               (identifier=? (the 'syntax-unquote",
"-splicing) (make-identifier (caar form) env))))\n\n        (define (qq depth expr)",
"\n          (cond\n           ;; syntax-unquote\n           ((syntax-unquote? expr)",
"\n            (if (= depth 1)\n                (car (cdr expr))\n                (l",
"ist (the 'list)\n                      (list (the 'quote) (the 'syntax-unquote))\n",
"                      (qq (- depth 1) (car (cdr expr))))))\n           ;; syntax-",
"unquote-splicing\n           ((syntax-unquote-splicing? expr)\n            (if (= ",
"depth 1)\n                (list (the 'append)\n                      (car (cdr (ca",
"r expr)))\n                      (qq depth (cdr expr)))\n                (list (th",
"e 'cons)\n                      (list (the 'list)\n                            (li",
"st (the 'quote) (the 'syntax-unquote-splicing))\n                            (qq ",
"(- depth 1) (car (cdr (car expr)))))\n                      (qq depth (cdr expr))",
")))\n           ;; syntax-quasiquote\n           ((syntax-quasiquote? expr)\n      ",
"      (list (the 'list)\n                  (list (the 'quote) (the 'quasiquote))\n",
"                  (qq (+ depth 1) (car (cdr expr)))))\n           ;; list\n       ",
"    ((pair? expr)\n            (list (the 'cons)\n                  (qq depth (car",
" expr))\n                  (qq depth (cdr expr))))\n           ;; vector\n         ",
"  ((vector? expr)\n            (list (the 'list->vector) (qq depth (vector->list ",
"expr))))\n           ;; identifier\n           ((identifier? expr)\n            (re",
"name expr))\n           ;; simple datum\n           (else\n            (list (the '",
"quote) expr))))\n\n        (let ((body (qq 1 (cadr form))))\n          `(,(the 'let",
")\n            ,(map cdr renames)\n            ,body))))))\n\n(define (transformer f",
")\n  (lambda (form env)\n    (let ((ephemeron1 (make-ephemeron))\n          (epheme",
"ron2 (make-ephemeron)))\n      (letrec\n          ((wrap (lambda (var1)\n          ",
"         (let ((var2 (ephemeron1 var1)))\n                     (if var2\n         ",
"                (cdr var2)\n                         (let ((var2 (make-identifier",
" var1 env)))\n                           (ephemeron1 var1 var2)\n                 ",
"          (ephemeron2 var2 var1)\n                           var2)))))\n          ",
" (unwrap (lambda (var2)\n                     (let ((var1 (ephemeron2 var2)))\n   ",
"                    (if var1\n                           (cdr var1)\n             ",
"              var2))))\n           (walk (lambda (f form)\n                   (con",
"d\n                    ((identifier? form)\n                     (f form))\n       ",
"             ((pair? form)\n                     (cons (walk f (car form)) (walk ",
"f (cdr form))))\n                    ((vector? form)\n                     (list->",
"vector (walk f (vector->list form))))\n                    (else\n                ",
"     form)))))\n        (let ((form (cdr form)))\n          (walk unwrap (apply f ",
"(walk wrap form))))))))\n\n(define-macro define-syntax\n  (lambda (form env)\n    (l",
"et ((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      (if (pa",
"ir? formal)\n          `(,(the 'define-syntax) ,(car formal) (,the-lambda ,(cdr f",
"ormal) ,@body))\n          `(,the-define-macro ,formal (,(the 'transformer) (,the",
"-begin ,@body)))))))\n\n(define-macro letrec-syntax\n  (lambda (form env)\n    (let ",
"((formal (car (cdr form)))\n          (body   (cdr (cdr form))))\n      `(let ()\n ",
"        ,@(map (lambda (x)\n                  `(,(the 'define-syntax) ,(car x) ,(",
"cadr x)))\n                formal)\n         ,@body))))\n\n(define-macro let-syntax\n",
"  (lambda (form env)\n    `(,(the 'letrec-syntax) ,@(cdr form))))\n\n\n;;; library p",
"rimitives\n\n(define (mangle name)\n  (when (null? name)\n    (error \"library name s",
"hould be a list of at least one symbols\" name))\n\n  (define (->string n)\n    (con",
"d\n     ((symbol? n)\n      (let ((str (symbol->string n)))\n        (string-for-ea",
"ch\n         (lambda (c)\n           (when (or (char=? c #\\.) (char=? c #\\/))\n    ",
"         (error \"elements of library name may not contain '.' or '/'\" n)))\n     ",
"    str)\n        str))\n     ((and (number? n) (exact? n))\n      (number->string ",
"n))\n     (else\n      (error \"symbol or integer is required\" n))))\n\n  (define (jo",
"in strs delim)\n    (let loop ((res (car strs)) (strs (cdr strs)))\n      (if (nul",
"l? strs)\n          res\n          (loop (string-append res delim (car strs)) (cdr",
" strs)))))\n\n  (join (map ->string name) \".\"))\n\n(define-macro define-library\n  (l",
"ambda (form _)\n    (let ((lib (mangle (cadr form)))\n          (body (cddr form))",
")\n      (or (find-library lib) (make-library lib))\n      (for-each (lambda (expr",
") (eval expr lib)) body))))\n\n(define-macro cond-expand\n  (lambda (form _)\n    (l",
"etrec\n        ((test (lambda (form)\n                 (or\n                  (eq? ",
"form 'else)\n                  (and (symbol? form)\n                       (memq f",
"orm (features)))\n                  (and (pair? form)\n                       (cas",
"e (car form)\n                         ((library) (find-library (mangle (cadr for",
"m))))\n                         ((not) (not (test (cadr form))))\n                ",
"         ((and) (let loop ((form (cdr form)))\n                                  ",
"(or (null? form)\n                                      (and (test (car form)) (l",
"oop (cdr form))))))\n                         ((or) (let loop ((form (cdr form)))",
"\n                                 (and (pair? form)\n                            ",
"          (or (test (car form)) (loop (cdr form))))))\n                         (",
"else #f)))))))\n      (let loop ((clauses (cdr form)))\n        (if (null? clauses",
")\n            #undefined\n            (if (test (caar clauses))\n                `",
"(,the-begin ,@(cdar clauses))\n                (loop (cdr clauses))))))))\n\n(defin",
"e-macro import\n  (lambda (form _)\n    (let ((caddr\n           (lambda (x) (car (",
"cdr (cdr x)))))\n          (prefix\n           (lambda (prefix symbol)\n           ",
"  (string->symbol\n              (string-append\n               (symbol->string pr",
"efix)\n               (symbol->string symbol)))))\n          (getlib\n           (l",
"ambda (name)\n             (let ((lib (mangle name)))\n               (if (find-li",
"brary lib)\n                   lib\n                   (error \"library not found\" ",
"name))))))\n      (letrec\n          ((extract\n            (lambda (spec)\n        ",
"      (case (car spec)\n                ((only rename prefix except)\n            ",
"     (extract (cadr spec)))\n                (else\n                 (getlib spec)",
"))))\n           (collect\n            (lambda (spec)\n              (case (car spe",
"c)\n                ((only)\n                 (let ((alist (collect (cadr spec))))",
"\n                   (map (lambda (var) (assq var alist)) (cddr spec))))\n        ",
"        ((rename)\n                 (let ((alist (collect (cadr spec)))\n         ",
"              (renames (map (lambda (x) `((car x) . (cadr x))) (cddr spec))))\n  ",
"                 (map (lambda (s) (or (assq (car s) renames) s)) alist)))\n      ",
"          ((prefix)\n                 (let ((alist (collect (cadr spec))))\n      ",
"             (map (lambda (s) (cons (prefix (caddr spec) (car s)) (cdr s))) alis",
"t)))\n                ((except)\n                 (let ((alist (collect (cadr spec",
"))))\n                   (let loop ((alist alist))\n                     (if (null",
"? alist)\n                         '()\n                         (if (memq (caar a",
"list) (cddr spec))\n                             (loop (cdr alist))\n             ",
"                (cons (car alist) (loop (cdr alist))))))))\n                (else",
"\n                 (map (lambda (x) (cons x x)) (library-exports (getlib spec))))",
"))))\n        (letrec\n            ((import\n               (lambda (spec)\n        ",
"         (let ((lib (extract spec))\n                       (alist (collect spec)",
"))\n                   (for-each\n                    (lambda (slot)\n             ",
"         (library-import lib (cdr slot) (car slot)))\n                    alist))",
")))\n          (for-each import (cdr form)))))))\n\n(define-macro export\n  (lambda ",
"(form _)\n    (letrec\n        ((collect\n          (lambda (spec)\n            (con",
"d\n             ((symbol? spec)\n              `(,spec . ,spec))\n             ((an",
"d (list? spec) (= (length spec) 3) (eq? (car spec) 'rename))\n              `(,(l",
"ist-ref spec 1) . ,(list-ref spec 2)))\n             (else\n              (error \"",
"malformed export\")))))\n         (export\n           (lambda (spec)\n             (",
"let ((slot (collect spec)))\n               (library-export (car slot) (cdr slot)",
")))))\n      (for-each export (cdr form)))))\n\n(export define lambda quote set! if",
" begin define-macro\n        let let* letrec letrec*\n        let-values let*-valu",
"es define-values\n        quasiquote unquote unquote-splicing\n        and or\n    ",
"    cond case else =>\n        do when unless\n        parameterize\n        define",
"-syntax\n        syntax-quote syntax-unquote\n        syntax-quasiquote syntax-unq",
"uote-splicing\n        let-syntax letrec-syntax\n        syntax-error)\n\n\n",
"",
""
};

void
pic_boot(pic_state *pic)
{
  pic_load_cstr(pic, &boot_rom[0][0]);
}

#if 0
Local Variables:
mode: scheme
End:

=cut
#endif
