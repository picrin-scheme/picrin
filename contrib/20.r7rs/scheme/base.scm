(define-library (scheme base)
  (import (picrin base)
          (only (picrin math)
                abs
                expt
                floor/
                truncate/
                floor
                ceiling
                truncate
                round
                sqrt
                nan?
                infinite?)
          (picrin macro)
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

  (define-macro include
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
          (let ((clause (car clauses))
                (rest (cdr clauses)))
            (cond
             ((else? clause)
              #`(begin #,@(cdr clause)))
             ((=>? clause)
              #`(let ((tmp #,(list-ref clause 0)))
                  (if tmp
                      (#,(list-ref clause 2) tmp)
                      (guard-aux #,reraise #,@rest))))
             ((= (length clause) 1)
              #`(or #,(car clause) (guard-aux #,reraise #,@rest)))
             (else
              #`(if #,(car clause)
                    (begin #,@(cdr clause))
                    (guard-aux #,reraise #,@rest))))))))

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

  (define (succ n)
    (+ n 1))

  (define (pred n)
    (if (= n 0)
        0
        (- n 1)))

  (define (every? args)
    (if (null? args)
        #t
        (if (car args)
            (every? (cdr args))
            #f)))

  (define (filter f list)
    (if (null? list)
        '()
        (if (f (car list))
            (cons (car list)
                  (filter f (cdr list)))
            (filter f (cdr list)))))

  (define (take-tail n list)
    (let drop ((n (- (length list) n)) (list list))
      (if (= n 0)
          list
          (drop (- n 1) (cdr list)))))

  (define (drop-tail n list)
    (let take ((n (- (length list) n)) (list list))
      (if (= n 0)
          '()
          (cons (car list) (take (- n 1) (cdr list))))))

  (define (map-keys f assoc)
    (map (lambda (s) `(,(f (car s)) . ,(cdr s))) assoc))

  (define (map-values f assoc)
    (map (lambda (s) `(,(car s) . ,(f (cdr s)))) assoc))

  ;; TODO
  ;; - placeholder
  ;; - vector
  ;; - (... template) pattern

  ;; p ::= constant
  ;;     | var
  ;;     | (p ... . p)      (in input pattern, tail p should be a proper list)
  ;;     | (p . p)

  (define (compile ellipsis literals rules)

    (define (constant? obj)
      (and (not (pair? obj))
           (not (identifier? obj))))

    (define (literal? obj)
      (and (identifier? obj)
           (memq obj literals)))

    (define (many? pat)
      (and (pair? pat)
           (pair? (cdr pat))
           (identifier? (cadr pat))
           (identifier=? (cadr pat) ellipsis)))

    (define (pattern-validator pat)      ; pattern -> validator
      (letrec
          ((pattern-validator
            (lambda (pat form)
              (cond
               ((constant? pat)
                #`(equal? '#,pat #,form))
               ((literal? pat)
                #`(and (identifier? #,form) (identifier=? #'#,pat #,form)))
               ((identifier? pat)
                #t)
               ((many? pat)
                (let ((head #`(drop-tail #,(length (cddr pat)) #,form))
                      (tail #`(take-tail #,(length (cddr pat)) #,form)))
                  #`(and (list? #,form)
                         (>= (length #,form) #,(length (cddr pat)))
                         (every? (map (lambda (#,'it) #,(pattern-validator (car pat) 'it)) #,head))
                         #,(pattern-validator (cddr pat) tail))))
               ((pair? pat)
                #`(and (pair? #,form)
                       #,(pattern-validator (car pat) #`(car #,form))
                       #,(pattern-validator (cdr pat) #`(cdr #,form))))
               (else
                #f)))))
        (pattern-validator pat 'it)))

    (define (pattern-variables pat)       ; pattern -> (freevar)
      (cond
       ((constant? pat)
        '())
       ((literal? pat)
        '())
       ((identifier? pat)
        `(,pat))
       ((many? pat)
        (append (pattern-variables (car pat))
                (pattern-variables (cddr pat))))
       ((pair? pat)
        (append (pattern-variables (car pat))
                (pattern-variables (cdr pat))))))

    (define (pattern-levels pat)          ; pattern -> ((var * int))
      (cond
       ((constant? pat)
        '())
       ((literal? pat)
        '())
       ((identifier? pat)
        `((,pat . 0)))
       ((many? pat)
        (append (map-values succ (pattern-levels (car pat)))
                (pattern-levels (cddr pat))))
       ((pair? pat)
        (append (pattern-levels (car pat))
                (pattern-levels (cdr pat))))))

    (define (pattern-selectors pat)       ; pattern -> ((var * selector))
      (letrec
          ((pattern-selectors
            (lambda (pat form)
              (cond
               ((constant? pat)
                '())
               ((literal? pat)
                '())
               ((identifier? pat)
                `((,pat . ,form)))
               ((many? pat)
                (let ((head #`(drop-tail #,(length (cddr pat)) #,form))
                      (tail #`(take-tail #,(length (cddr pat)) #,form)))
                  (let ((envs (pattern-selectors (car pat) 'it)))
                    (append
                     (map-values (lambda (s) #`(map (lambda (#,'it) #,s) #,head)) envs)
                     (pattern-selectors (cddr pat) tail)))))
               ((pair? pat)
                (append (pattern-selectors (car pat) #`(car #,form))
                        (pattern-selectors (cdr pat) #`(cdr #,form))))))))
        (pattern-selectors pat 'it)))

    (define (template-representation pat levels selectors)
      (cond
       ((constant? pat)
        pat)
       ((identifier? pat)
        (let ((it (assq pat levels)))
          (if it
              (if (= 0 (cdr it))
                  (cdr (assq pat selectors))
                  (error "unmatched pattern variable level" pat))
              #`(#,'rename '#,pat))))
       ((many? pat)
        (letrec*
            ((inner-pat
              (car pat))
             (inner-levels
              (map (lambda (s) `(,(car s) . ,(pred (cdr s)))) levels))
             (inner-freevars
              (filter (lambda (v) (assq v levels)) (pattern-variables inner-pat)))
             (inner-vars
              ;; select only vars declared with ellipsis
              (filter (lambda (v) (> (cdr (assq v levels)) 0)) inner-freevars))
             (inner-tmps
              (map (lambda (v) #'it) inner-vars))
             (inner-selectors
              ;; first env '(map cons ...)' shadows second env 'selectors'
              (append (map cons inner-vars inner-tmps) selectors))
             (inner-rep
              (template-representation inner-pat inner-levels inner-selectors))
             (sorted-selectors
              (map (lambda (v) (assq v selectors)) inner-vars))
             (list-of-selectors
              ;; ((a . xs) (b . ys) (c . zs)) -> (xs ys zs)
              (map cdr sorted-selectors)))
          (let ((rep1 #`(map (lambda #,inner-tmps #,inner-rep) #,@list-of-selectors))
                (rep2 (template-representation (cddr pat) levels selectors)))
            #`(append #,rep1 #,rep2))))
       ((pair? pat)
        #`(cons #,(template-representation (car pat) levels selectors)
                #,(template-representation (cdr pat) levels selectors)))))

    (define (compile-rule pattern template)
      (let ((levels
             (pattern-levels pattern))
            (selectors
             (pattern-selectors pattern)))
        (template-representation template levels selectors)))

    (define (compile-rules rules)
      (if (null? rules)
          #`(error "unmatch")
          (let ((pattern (car (car rules)))
                (template (cadr (car rules))))
            #`(if #,(pattern-validator pattern)
                  #,(compile-rule pattern template)
                  #,(compile-rules (cdr rules))))))

    (define (compile rules)
      #`(call-with-current-environment
         (lambda (env)
           (letrec
               ((#,'rename (let ((wm (make-ephemeron)))
                             (lambda (x)
                               (let ((y (wm x)))
                                 (if y
                                     (cdr y)
                                     (let ((id (make-identifier x env)))
                                       (wm x id)
                                       id)))))))
             (lambda #,'it
               #,(compile-rules rules))))))

    (let ((rules (map-keys cdr rules))) ; TODO: check pattern head is a variable
      (compile rules)))

  (define-syntax (syntax-rules . args)
    (if (list? (car args))
        #`(syntax-rules ... #,@args)
        (let ((ellipsis (car args))
              (literals (car (cdr args)))
              (rules    (cdr (cdr args))))
          (compile ellipsis literals rules))))

  (define-syntax (define-auxiliary-syntax var)
    #`(define-macro #,var
        (lambda _
          (error "invalid use of auxiliary syntax" '#,var))))

  (define-auxiliary-syntax _)
  (define-auxiliary-syntax ...)

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

  ;; 5.5 Record-type definitions

  (define (make-record-type name)
    (vector name))                      ; TODO

  (define-syntax (define-record-constructor type field-alist name . fields)
    (let ((record #'record))
      #`(define (#,name . #,fields)
          (let ((#,record (make-record #,type (make-vector #,(length field-alist)))))
            #,@(map
                (lambda (field)
                  #`(vector-set! (record-datum #,record) #,(cdr (assq field field-alist)) #,field)) 
                fields)
            #,record))))

  (define-syntax (define-record-predicate type name)
    #`(define (#,name obj)
        (and (record? obj)
             (eq? (record-type obj) #,type))))

  (define-syntax (define-record-accessor pred field-alist field accessor)
    #`(define (#,accessor record)
        (if (#,pred record)
            (vector-ref (record-datum record) #,(cdr (assq field field-alist)))
            (error (string-append (symbol->string  '#,accessor) ": wrong record type") record))))

  (define-syntax (define-record-modifier pred field-alist field modifier)
    #`(define (#,modifier record val)
        (if (#,pred record)
            (vector-set! (record-datum record) #,(cdr (assq field field-alist)) val)
            (error (string-append (symbol->string '#,modifier) ": wrong record type")  record))))

  (define-syntax (define-record-field pred field-alist field accessor . modifier-opt)
    (if (null? modifier-opt)
        #`(define-record-accessor #,pred #,field-alist #,field #,accessor)
        #`(begin
            (define-record-accessor #,pred #,field-alist #,field #,accessor)
            (define-record-modifier #,pred #,field-alist #,field #,(car modifier-opt)))))

  (define-syntax (define-record-type name ctor pred . fields)
    (let ((field-alist (let lp ((fds fields) (idx 0) (alst '()))
                            (if (null? fds)
                              alst
                              (lp (cdr fds)
                                  (+ idx 1)
                                  (cons
                                    (cons (if (pair? (car fds)) (car (car fds)) (car fds)) idx)
                                    alst))))))
      #`(begin
          (define #,name (make-record-type '#,name))
          (define-record-constructor #,name #,field-alist #,@ctor)
          (define-record-predicate #,name #,pred)
          #,@(map (lambda (field) #`(define-record-field #,pred #,field-alist #,@field)) fields))))

  (export define-record-type)

  ;; 6.1. Equivalence predicates

  (export eq?
          eqv?
          equal?)

  ;; 6.2. Numbers

  (define complex? number?)
  (define real? number?)
  (define rational? number?)
  (define (integer? o)
    (or (exact? o)
        (and (inexact? o)
             (not (nan? o))
             (not (infinite? o))
             (= o (floor o)))))

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

  (define (input-port-open? port)
    (and (input-port? port) (port-open? port)))

  (define (output-port-open? port)
    (and (output-port? port) (port-open? port)))

  (define (call-with-port port handler)
    (let ((res (handler port)))
      (close-port port)
      res))

  (define (open-input-string str)
    (open-input-bytevector (list->bytevector (map char->integer (string->list str)))))

  (define (open-output-string)
    (open-output-bytevector))

  (define (get-output-string port)
    (list->string (map integer->char (bytevector->list (get-output-bytevector port)))))

  (define (read-char . opt)
    (let ((b (apply read-u8 opt)))
      (if (eof-object? b)
          b
          (integer->char b))))

  (define (peek-char . opt)
    (let ((b (apply peek-u8 opt)))
      (if (eof-object? b)
          b
          (integer->char b))))

  (define (u8-ready? . opt)
    #t)

  (define (read-bytevector k . opt)
    (let ((port (if (null? opt) (current-input-port) (car opt))))
      (let ((buf (make-bytevector k)))
        (let ((n (read-bytevector! buf port 0 k)))
          (if (eof-object? n)
              (eof-object)
              (bytevector-copy buf 0 n))))))

  (define (char-ready? . opt)
    #t)

  (define (newline . opt)
    (apply write-u8 (char->integer #\newline) opt))

  (define (write-char c . opt)
    (apply write-u8 (char->integer c) opt))

  (define (write-string s . opt)
    (apply write-bytevector (list->bytevector (map char->integer (string->list s))) opt))

  (define (read-line . opt)
    (if (eof-object? (apply peek-char opt))
        (eof-object)
        (let loop ((str "") (c (apply read-char opt)))
          (if (or (eof-object? c)
                  (char=? c #\newline))
              str
              (loop (string-append str (string c)) (apply read-char opt))))))

  (define (read-string k . opt)
    (if (eof-object? (apply peek-char opt))
        (eof-object)
        (let loop ((k k) (str "") (c (apply read-char opt)))
          (if (or (eof-object? c)
                  (zero? k))
              str
              (loop (- k 1) (string-append str (string c)) (apply read-char opt))))))

  (export current-input-port
          current-output-port
          current-error-port

          call-with-port

          port?
          input-port?
          output-port?
          (rename port? textual-port?)
          (rename port? binary-port?)

          input-port-open?
          output-port-open?
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
