(define-library (picrin base)
  (import (picrin macro))

  (define-syntax syntax-error
    (er-macro-transformer
     (lambda (expr rename compare)
       (apply error (cdr expr)))))

  (define-syntax define-auxiliary-syntax
    (er-macro-transformer
     (lambda (expr r c)
       (list (r 'define-syntax) (cadr expr)
             (list (r 'lambda) '_
                   (list (r 'error) "invalid use of auxiliary syntax"))))))

  (define-auxiliary-syntax else)
  (define-auxiliary-syntax =>)
  (define-auxiliary-syntax _)
  (define-auxiliary-syntax ...)
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
                    `((,(r 'define) ,(car formal) #f) . ,@(loop (cdr formal)))))
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

  (define-syntax include
    (letrec ((read-file
              (lambda (filename)
                (let ((port (open-input-file filename)))
                  (dynamic-wind
                      (lambda () #f)
                      (lambda ()
                        (let loop ((expr (read port)) (exprs '()))
                          (if (eof-object? expr)
                              (reverse exprs)
                              (loop (read port) (cons expr exprs)))))
                      (lambda ()
                        (close-port port)))))))
      (er-macro-transformer
       (lambda (form rename compare)
         (let ((filenames (cdr form)))
           (let ((exprs (apply append (map read-file filenames))))
             `(,(rename 'begin) ,@exprs)))))))

  (export define
          lambda
          if
          quote
          set!
          begin
          define-syntax)

  (export let let* letrec letrec*
          let-values let*-values define-values
          quasiquote unquote unquote-splicing
          and or
          cond case else =>
          do when unless
          let-syntax letrec-syntax
          include
          _ ... syntax-error)

  (export eq?
          eqv?
          equal?)

  (export boolean?
          boolean=?
          not)

  (export symbol?
          symbol->string
          string->symbol
          symbol=?)

  (export char?
          char->integer
          integer->char)

  (export number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          =
          <
          >
          <=
          >=
          +
          -
          *
          /
          abs
          floor/
          truncate/
          floor
          ceiling
          truncate
          round
          expt
          number->string
          string->number
          finite?
          infinite?
          nan?
          exp
          log
          sin
          cos
          tan
          acos
          asin
          atan
          sqrt)

  (export pair?
          cons
          car
          cdr
          set-car!
          set-cdr!
          null?
          caar
          cadr
          cdar
          cddr)

  (export list?
          make-list
          list
          length
          append
          reverse
          list-tail
          list-ref
          list-set!
          list-copy
          memq
          memv
          member
          assq
          assv
          assoc)

  (export bytevector?
          make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-copy!
          bytevector-append)

  (export vector?
          make-vector
          vector-length
          vector-ref
          vector-set!
          vector-copy!
          vector-copy
          vector-append
          vector-fill!
          list->vector
          vector->list)

  (export string?
          string-length
          string-ref
          string-copy
          string-append
          string=?
          string<?
          string>?
          string<=?
          string>=?)

  (export make-dictionary
          dictionary?
          dictionary-ref
          dictionary-set!
          dictionary-delete
          dictionary-size
          dictionary-for-each)

  (export make-record
          record?
          record-type
          record-ref
          record-set!)

  (export current-input-port
          current-output-port
          current-error-port

          port?
          input-port?
          output-port?
          textual-port?
          binary-port?

          close-port

          open-input-file
          open-output-file
          open-binary-input-file
          open-binary-output-file
          open-input-string
          open-output-string
          get-output-string
          open-input-bytevector
          open-output-bytevector
          get-output-bytevector

          eof-object?
          eof-object

          read-char
          peek-char
          char-ready?
          read-line
          read-string

          read-u8
          peek-u8
          u8-ready?
          read-bytevector
          read-bytevector!

          newline
          write-char
          write-string
          write-u8
          write-bytevector
          flush-output-port)

  (export make-parameter
          parameter-ref
          parameter-set!
          parameter-push!
          parameter-pop!)

  (export identifier?
          identifier=?
          make-identifier)

  (export call-with-current-continuation
          call/cc
          continue
          dynamic-wind
          values
          call-with-values)

  (export with-exception-handler
          raise
          raise-continuable
          error
          error-object?
          error-object-message
          error-object-irritants
          read-error?
          file-error?)

  (export procedure?
          apply
          map
          for-each
          attribute)

  (export read)

  (export write
          write-simple
          write-shared
          display)

  (export command-line
          exit
          emergency-exit
          file-exists?
          delete-file
          get-environment-variable
          get-environment-variables)

  (export current-second
          current-jiffy
          jiffies-per-second)

  (export eval)

  (export load))
