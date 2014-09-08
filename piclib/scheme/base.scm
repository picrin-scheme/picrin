(define-library (scheme base)
  (import (picrin base)
          (picrin macro)
          (picrin record)
          (picrin syntax-rules))

  ;; 4.1.2. Literal expressions

  (export quote)

  ;; 4.1.4. Procedures

  (export lambda)

  ;; 4.1.5. Conditionals

  (export if)

  ;; 4.1.6. Assignments

  (export set!)

  ;; 4.1.7. Inclusion

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

  (define-syntax parameterize
    (ir-macro-transformer
     (lambda (form inject compare)
       (let ((formal (car (cdr form)))
             (body   (cdr (cdr form))))
         (let ((vars (map car formal))
               (vals (map cadr formal)))
           `(begin
              ,@(map (lambda (var val) `(parameter-push! ,var ,val)) vars vals)
              (let ((result (begin ,@body)))
                ,@(map (lambda (var) `(parameter-pop! ,var)) vars)
                result)))))))

  (export make-parameter
          parameterize)

  ;; 4.2.7. Exception handling

  (define-syntax guard-aux
    (syntax-rules (else =>)
      ((guard-aux reraise (else result1 result2 ...))
       (begin result1 result2 ...))
      ((guard-aux reraise (test => result))
       (let ((temp test))
         (if temp
             (result temp)
             reraise)))
      ((guard-aux reraise (test => result)
                  clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (guard-aux reraise clause1 clause2 ...))))
      ((guard-aux reraise (test))
       (or test reraise))
      ((guard-aux reraise (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (guard-aux reraise clause1 clause2 ...))))
      ((guard-aux reraise (test result1 result2 ...))
       (if test
           (begin result1 result2 ...)
           reraise))
      ((guard-aux reraise
                  (test result1 result2 ...)
                  clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (guard-aux reraise clause1 clause2 ...)))))

  (define-syntax guard
    (syntax-rules ()
      ((guard (var clause ...) e1 e2 ...)
       ((call/cc
         (lambda (guard-k)
           (with-exception-handler
            (lambda (condition)
              ((call/cc
                (lambda (handler-k)
                  (guard-k
                   (lambda ()
                     (let ((var condition))
                       (guard-aux
                        (handler-k
                         (lambda ()
                           (raise-continuable condition)))
                        clause ...))))))))
            (lambda ()
              (call-with-values
                  (lambda () e1 e2 ...)
                (lambda args
                  (guard-k
                   (lambda ()
                     (apply values args)))))))))))))

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
    (let loop ((args args) (min +inf.0))
      (if (null? args)
          min
          (loop (cdr args) (if (< (car args) min)
                               (car args)
                               min)))))

  (define (max . args)
    (let loop ((args args) (max -inf.0))
      (if (null? args)
          max
          (loop (cdr args) (if (> (car args) max)
                               (car args)
                               max)))))

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

  (define (gcd i j)
    (if (> i j)
        (gcd j i)
        (if (zero? i)
            j
            (gcd (truncate-remainder j i) i))))

  (define (lcm i j)
    (/ (* i j) (gcd i j)))

  (define (square x)
    (* x x))

  (define (exact-integer-sqrt k)
    (let ((s (exact (sqrt k))))
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

  (define-macro (define-char-transitive-predicate name op)
    `(define (,name . cs)
       (apply ,op (map char->integer cs))))

  (define-char-transitive-predicate char=? =)
  (define-char-transitive-predicate char<? <)
  (define-char-transitive-predicate char>? >)
  (define-char-transitive-predicate char<=? <=)
  (define-char-transitive-predicate char>=? >=)

  (export char?
          char->integer
          integer->char
          char=?
          char<?
          char>?
          char<=?
          char>=?)

  ;; 6.7. Strings

  (define (string->list string . opts)
    (let ((start (if (pair? opts) (car opts) 0))
          (end (if (>= (length opts) 2)
                   (cadr opts)
                   (string-length string))))
      (do ((i start (+ i 1))
           (res '()))
          ((= i end)
           (reverse res))
        (set! res (cons (string-ref string i) res)))))

  (define (list->string list)
    (let ((len (length list)))
      (let ((v (make-string len)))
        (do ((i 0 (+ i 1))
             (l list (cdr l)))
            ((= i len)
             v)
          (string-set! v i (car l))))))

  (define (string . objs)
    (list->string objs))

  (export string?
          string
          make-string
          string-length
          string-ref
          string-set!
          string-copy
          string-copy!
          string-append
          string-fill!
          string=?
          string<?
          string>?
          string<=?
          string>=?
          string->list
          list->string
          (rename string-copy substring))

  ;; 6.8. Vectors

  (define (vector . objs)
    (list->vector objs))

  (define (vector->string . args)
    (list->string (apply vector->list args)))

  (define (string->vector . args)
    (list->vector (apply string->list args)))

  (export vector
          vector->string
          string->vector)

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

  ;; 6.9. bytevector

  (define (bytevector->list v start end)
    (do ((i start (+ i 1))
         (res '()))
        ((= i end)
         (reverse res))
      (set! res (cons (bytevector-u8-ref v i) res))))

  (define (list->bytevector list)
    (let ((len (length list)))
      (let ((v (make-bytevector len)))
        (do ((i 0 (+ i 1))
             (l list (cdr l)))
            ((= i len)
             v)
          (bytevector-u8-set! v i (car l))))))

  (define (bytevector . objs)
    (list->bytevector objs))

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

  (export bytevector
          bytevector->list
          list->bytevector
          utf8->string
          string->utf8)

  (export bytevector?
          make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-copy!
          bytevector-append)

  ;; 6.10. Control features

  (define (string-map f . strings)
    (list->string (apply map f (map string->list strings))))

  (define (string-for-each f . strings)
    (apply for-each f (map string->list strings)))

  (define (vector-map f . vectors)
    (list->vector (apply map f (map vector->list vectors))))

  (define (vector-for-each f . vectors)
    (apply for-each f (map vector->list vectors)))

  (export string-map
          string-for-each
          vector-map
          vector-for-each)

  (export procedure?
          apply
          map
          for-each
          call-with-current-continuation
          call/cc
          dynamic-wind
          values
          call-with-values)

  ;; 6.11. Exceptions

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

  (define (call-with-port port proc)
    (dynamic-wind
        (lambda () #f)
        (lambda () (proc port))
        (lambda () (close-port port))))

  (export current-input-port
          current-output-port
          current-error-port

          call-with-port

          port?
          input-port?
          output-port?
          textual-port?
          binary-port?

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
          flush-output-port))
