(define-library (scheme base)
  (import (picrin base)
          (picrin macro)
          (picrin syntax-rules)
          (picrin string)
          (scheme file))

  ;; 4.1.2. Literal expressions

  (export quote)

  ;; 4.1.4. Procedures

  (export lambda)

  ;; 4.1.5. Conditionals

  (export if)

  ;; 4.1.6. Assignments

  (export set!)

  ;; 4.1.7. Inclusion

  (define-syntax include
    (letrec ((read-file
              (lambda (filename)
                (call-with-port (open-input-file filename)
                  (lambda (port)
                    (let loop ((expr (read port)) (exprs '()))
                      (if (eof-object? expr)
                          (reverse exprs)
                          (loop (read port) (cons expr exprs)))))))))
      (er-macro-transformer
       (lambda (form rename compare)
         (let ((filenames (cdr form)))
           (let ((exprs (apply append (map read-file filenames))))
             `(,(rename 'begin) ,@exprs)))))))

  (export include)

  ;; 4.2.1. Conditionals

  (export cond
          case
          else
          =>
          and
          or
          when
          unless)

  ;; 4.2.2. Binding constructs

  (export let
          let*
          letrec
          letrec*
          let-values
          let*-values)

  ;; 4.2.3. Sequencing

  (export begin)

  ;; 4.2.4. Iteration

  (export do)

  ;; 4.2.6. Dynamic bindings

  (export make-parameter
          parameterize)

  ;; 4.2.7. Exception handling

  (define-syntax (guard-aux reraise . clauses)
    (letrec
        ((else?
          (lambda (clause)
            (and (list? clause) (equal? #'else (car clause)))))
         (=>?
          (lambda (clause)
            (and (list? clause) (= (length clause) 3) (equal? #'=> (list-ref clause 1))))))
      (if (null? clauses)
          reraise
          (let ((clause (car clauses)))
            (cond
             ((else? clause)
              #`(begin #,@(cdr clause)))
             ((=>? clause)
              #`(let ((tmp #,(list-ref clause 0)))
                  (if tmp
                      (#,(list-ref clause 2) tmp)
                      (guard-aux #,reraise #,@(cdr clauses)))))
             (else
              #`(if #,(car clause)
                    (begin #,@(cdr clause))
                    (guard-aux #,reraise #,@(cdr clauses)))))))))

  (define-syntax (guard formal . body)
    (let ((var (car formal))
          (clauses (cdr formal)))
      #`((call/cc
          (lambda (guard-k)
            (with-exception-handler
             (lambda (condition)
               ((call/cc
                 (lambda (handler-k)
                   (guard-k
                    (lambda ()
                      (let ((#,var condition))
                        (guard-aux
                         (handler-k
                          (lambda ()
                            (raise-continuable condition)))
                         #,@clauses))))))))
             (lambda ()
               (call-with-values
                   (lambda () #,@body)
                 (lambda args
                   (guard-k
                    (lambda ()
                      (apply values args))))))))))))

  (export guard)

  ;; 4.2.8. Quasiquotation

  (export quasiquote
          unquote
          unquote-splicing)

  ;; 4.3.1. Binding constructs for syntactic keywords

  (export let-syntax
          letrec-syntax)

  ;; 4.3.2 Pattern language

  (export syntax-rules
          _
          ...)

  ;; 4.3.3. Signaling errors in macro transformers

  (export syntax-error)

  ;; 5.3. Variable definitions

  (export define)

  ;; 5.3.3. Multiple-value definitions

  (export define-values)

  ;; 5.4. Syntax definitions

  (export define-syntax)

  ;; 5.5 Recored-type definitions

  (define ((boot-make-record-type <meta-type>) name)
    (let ((rectype (make-record <meta-type>)))
      (record-set! rectype 'name name)
      rectype))

  (define <record-type>
    (let ((<record-type> ((boot-make-record-type #t) 'record-type)))
      (record-set! <record-type> '@@type <record-type>)
      <record-type>))

  (define make-record-type (boot-make-record-type <record-type>))

  (define-syntax (define-record-constructor type name . fields)
    (let ((record #'record))
      #`(define (#,name . #,fields)
          (let ((#,record (make-record #,type)))
            #,@(map (lambda (field) #`(record-set! #,record '#,field #,field)) fields)
            #,record))))

  (define-syntax (define-record-predicate type name)
    #`(define (#,name obj)
        (and (record? obj)
             (eq? (record-type obj) #,type))))

  (define-syntax (define-record-accessor pred field accessor)
    #`(define (#,accessor record)
        (if (#,pred record)
            (record-ref record '#,field)
            (error (string-append (symbol->string  '#,accessor) ": wrong record type") record))))

  (define-syntax (define-record-modifier pred field modifier)
    #`(define (#,modifier record val)
        (if (#,pred record)
            (record-set! record '#,field val)
            (error (string-append (symbol->string '#,modifier) ": wrong record type")  record))))

  (define-syntax (define-record-field pred field accessor . modifier-opt)
    (if (null? modifier-opt)
        #`(define-record-accessor #,pred #,field #,accessor)
        #`(begin
            (define-record-accessor #,pred #,field #,accessor)
            (define-record-modifier #,pred #,field #,(car modifier-opt)))))

  (define-syntax (define-record-type name ctor pred . fields)
    #`(begin
        (define #,name (make-record-type '#,name))
        (define-record-constructor #,name #,@ctor)
        (define-record-predicate #,name #,pred)
        #,@(map (lambda (field) #`(define-record-field #,pred #,@field)) fields)))

  (export define-record-type)

  ;; 6.1. Equivalence predicates

  (export eq?
          eqv?
          equal?)

  ;; 6.2. Numbers

  (define (exact-integer? x)
    (and (exact? x)
         (integer? x)))

  (define (zero? x)
    (= x 0))

  (define (positive? x)
    (> x 0))

  (define (negative? x)
    (< x 0))

  (define (even? x)
    (= x (* (exact (floor (/ x 2))) 2)))

  (define (odd? x)
    (not (even? x)))

  (define (min . args)
    (define (min a b)
      (if (< a b) a b))
    (let loop ((args args) (acc +inf.0) (exactp #t))
      (if (null? args)
          (if exactp acc (inexact acc))
          (loop (cdr args) (min (car args) acc) (and (exact? (car args)) exactp)))))

  (define (max . args)
    (define (max a b)
      (if (> a b) a b))
    (let loop ((args args) (acc -inf.0) (exactp #t))
      (if (null? args)
          (if exactp acc (inexact acc))
          (loop (cdr args) (max (car args) acc) (and (exact? (car args)) exactp)))))

  (define (floor-quotient i j)
    (call-with-values (lambda () (floor/ i j))
      (lambda (q r)
        q)))

  (define (floor-remainder i j)
    (call-with-values (lambda () (floor/ i j))
      (lambda (q r)
        r)))

  (define (truncate-quotient i j)
    (call-with-values (lambda () (truncate/ i j))
      (lambda (q r)
        q)))

  (define (truncate-remainder i j)
    (call-with-values (lambda () (truncate/ i j))
      (lambda (q r)
        r)))

  (define (gcd . args)
    (define (gcd i j)
      (cond
       ((> i j) (gcd j i))
       ((< i 0) (gcd (- i) j))
       ((> i 0) (gcd (truncate-remainder j i) i))
       (else j)))
    (let loop ((args args) (acc 0))
      (if (null? args)
          acc
          (loop (cdr args)
                (gcd acc (car args))))))

  (define (lcm . args)
    (define (lcm i j)
      (/ (abs (* i j)) (gcd i j)))
    (let loop ((args args) (acc 1))
      (if (null? args)
          acc
          (loop (cdr args)
                (lcm acc (car args))))))

  (define (square x)
    (* x x))

  (define (exact-integer-sqrt k)
    (let ((s (exact (floor (sqrt k)))))
      (values s (- k (square s)))))

  (export number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          exact-integer?
          exact
          inexact
          =
          <
          >
          <=
          >=
          zero?
          positive?
          negative?
          odd?
          even?
          min
          max
          +
          -
          *
          /
          abs
          floor-quotient
          floor-remainder
          floor/
          truncate-quotient
          truncate-remainder
          truncate/
          (rename truncate-quotient quotient)
          (rename truncate-remainder remainder)
          (rename floor-remainder modulo)
          gcd
          lcm
          floor
          ceiling
          truncate
          round
          exact-integer-sqrt
          square
          expt
          number->string
          string->number)

  ;; 6.3. Booleans

  (export boolean?
          boolean=?
          not)

  ;; 6.4 Pairs and lists

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
          cddr
          list?
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

  ;; 6.5. Symbols

  (export symbol?
          symbol=?
          symbol->string
          string->symbol)

  ;; 6.6. Characters

  (export char?
          char->integer
          integer->char
          char=?
          char<?
          char>?
          char<=?
          char>=?)

  ;; 6.7. Strings

  (export string?
          string
          make-string
          string-length
          string-ref
          string-set!
          string-copy
          string-copy!
          string-append
          (rename string-copy substring)
          string-fill!
          string->list
          list->string
          string=?
          string<?
          string>?
          string<=?
          string>=?)

  ;; 6.8. Vectors

  (export vector?
          vector
          make-vector
          vector-length
          vector-ref
          vector-set!
          vector-copy!
          vector-copy
          vector-append
          vector-fill!
          list->vector
          vector->list
          string->vector
          vector->string)

  ;; 6.9. Bytevectors

  (define (utf8->string v . opts)
    (let ((start (if (pair? opts) (car opts) 0))
          (end (if (>= (length opts) 2)
                   (cadr opts)
                   (bytevector-length v))))
      (list->string (map integer->char (bytevector->list v start end)))))

  (define (string->utf8 s . opts)
    (let ((start (if (pair? opts) (car opts) 0))
          (end (if (>= (length opts) 2)
                   (cadr opts)
                   (string-length s))))
      (list->bytevector (map char->integer (string->list s start end)))))

  (export bytevector?
          bytevector
          make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-copy
          bytevector-copy!
          bytevector-append
          bytevector->list
          list->bytevector
          utf8->string
          string->utf8)

  ;; 6.10. Control features

  (export procedure?
          apply
          map
          for-each
          string-map
          string-for-each
          vector-map
          vector-for-each
          call-with-current-continuation
          call/cc
          dynamic-wind
          values
          call-with-values)

  ;; 6.11. Exceptions

  (define (read-error? obj)
    (and (error-object? obj)
         (eq? (error-object-type obj) 'read)))

  (define (file-error? obj)
    (and (error-object? obj)
         (eq? (error-object-type obj) 'file)))

  (export with-exception-handler
          raise
          raise-continuable
          error
          error-object?
          error-object-message
          error-object-irritants
          read-error?
          file-error?)

  ;; 6.13. Input and output

  (export current-input-port
          current-output-port
          current-error-port

          call-with-port

          port?
          input-port?
          output-port?
          textual-port?
          binary-port?

          (rename port-open? input-port-open?)
          (rename port-open? output-port-open?)
          close-port
          (rename close-port close-input-port)
          (rename close-port close-output-port)

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

  (export features))
