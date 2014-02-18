;;; Appendix A. Standard Libraries CxR
(define-library (scheme cxr)
  (import (scheme base))

  (define (caaar p) (car (caar p)))
  (define (caadr p) (car (cadr p)))
  (define (cadar p) (car (cdar p)))
  (define (caddr p) (car (cddr p)))
  (define (cdaar p) (cdr (caar p)))
  (define (cdadr p) (cdr (cadr p)))
  (define (cddar p) (cdr (cdar p)))
  (define (cdddr p) (cdr (cddr p)))
  (define (caaaar p) (caar (caar p)))
  (define (caaadr p) (caar (cadr p)))
  (define (caadar p) (caar (cdar p)))
  (define (caaddr p) (caar (cddr p)))
  (define (cadaar p) (cadr (caar p)))
  (define (cadadr p) (cadr (cadr p)))
  (define (caddar p) (cadr (cdar p)))
  (define (cadddr p) (cadr (cddr p)))
  (define (cdaaar p) (cdar (caar p)))
  (define (cdaadr p) (cdar (cadr p)))
  (define (cdadar p) (cdar (cdar p)))
  (define (cdaddr p) (cdar (cddr p)))
  (define (cddaar p) (cddr (caar p)))
  (define (cddadr p) (cddr (cadr p)))
  (define (cdddar p) (cddr (cdar p)))
  (define (cddddr p) (cddr (cddr p)))

  (export caaar caadr cadar caddr
          cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr
          cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr
          cddaar cddadr cdddar cddddr))

;;; hygienic macros
(define-library (picrin macro)
  (import (scheme base))

  (define (sc-macro-transformer f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env))))

  (define (rsc-macro-transformer f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure use-env '() (f expr mac-env))))

  (export sc-macro-transformer
          rsc-macro-transformer))

;;; core syntaces
(define-library (picrin core-syntax)
  (import (scheme base)
          (scheme cxr)
          (picrin macro))

  (define-syntax let
    (er-macro-transformer
     (lambda (expr r compare)
       (if (symbol? (cadr expr))
           (begin
             (define name (cadr expr))
             (define bindings (caddr expr))
             (define body (cdddr expr))
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
             (if (compare (r 'else) (caar clauses))
                 (cons (r 'begin) (cdar clauses))
                 (list (r 'if) (caar clauses)
                       (cons (r 'begin) (cdar clauses))
                       (cons (r 'cond) (cdr clauses)))))))))

  (define (single? list)
    (if (pair? list)
        (null? (cdr list))
        #f))

  (define-syntax and
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((exprs (cdr expr)))
         (cond
          ((null? exprs)
           #t)
          ((single? exprs)
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
          ((single? exprs)
           (car exprs))
          (else
           (list (r 'let) (list (list (r 'it) (car exprs)))
                 (list (r 'if) (r 'it)
                       (r 'it)
                       (cons (r 'or) (cdr exprs))))))))))

  (define (quasiquote? form compare?)
    (and (pair? form) (compare? (car form) 'quasiquote)))

  (define (unquote? form compare?)
    (and (pair? form) (compare? (car form) 'unquote)))

  (define (unquote-splicing? form compare?)
    (and (pair? form) (pair? (car form)) (compare? (car (car form)) 'unquote-splicing)))

  (define-syntax quasiquote
    (ir-macro-transformer
     (lambda (form inject compare)

       (define (qq depth expr)
         (cond
          ;; unquote
          ((unquote? expr compare)
           (if (= depth 1)
               (car (cdr expr))
               (list 'list
                     (list 'quote (inject 'unquote))
                     (qq (- depth 1) (car (cdr expr))))))
          ;; unquote-splicing
          ((unquote-splicing? expr compare)
           (if (= depth 1)
               (list 'append
                     (car (cdr (car expr)))
                     (qq depth (cdr expr)))
               (list 'cons
                     (list 'list
                           (list 'quote (inject 'unquote-splicing))
                           (qq (- depth 1) (car (cdr (car expr)))))
                     (qq depth (cdr expr)))))
          ;; quasiquote
          ((quasiquote? expr compare)
           (list 'list
                 (list 'quote (inject 'quasiquote))
                 (qq (+ depth 1) (car (cdr expr)))))
          ;; list
          ((pair? expr)
           (list 'cons
                 (qq depth (car expr))
                 (qq depth (cdr expr))))
          ;; simple datum
          (else
           (list 'quote expr))))

       (let ((x (cadr form)))
         (qq 1 x)))))

  #;
  (define-syntax let*
    (ir-macro-transformer
     (lambda (form inject compare)
       (let ((bindings (cadr form))
             (body (cddr form)))
         (if (null? bindings)
             `(let () ,@body)
             `(let ((,(caar bindings)
                     ,@(cdar bindings)))
                (let* (,@(cdr bindings))
                  ,@body)))))))

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

  (define-syntax do
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (cadr form))
             (finish (caddr form))
             (body (cdddr form)))
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
                   `(,(r 'if) (,(r 'or)
                               ,@(map (lambda (x) `(,(r 'eqv?) ,(r 'key) (,(r 'quote) ,x)))
                                      (caar clauses)))
                        (begin ,@(cdar clauses))
                        ,(loop (cdr clauses))))))))))

  (define-syntax syntax-error
    (er-macro-transformer
     (lambda (expr rename compare)
       (apply error (cdr expr)))))

  (define-syntax define-auxiliary-syntax
    (er-macro-transformer
     (lambda (expr r c)
       `(,(r 'define-syntax) ,(cadr expr)
           (,(r 'sc-macro-transformer)
                (,(r 'lambda) (expr env)
                  (,(r 'error) "invalid use of auxiliary syntax")))))))

  (define-auxiliary-syntax else)
  (define-auxiliary-syntax =>)
  (define-auxiliary-syntax _)
  (define-auxiliary-syntax ...)
  (define-auxiliary-syntax unquote)
  (define-auxiliary-syntax unquote-splicing)

  (export let let* letrec letrec*
          quasiquote unquote unquote-splicing
          and or
          cond case else =>
          do when unless
          _ ... syntax-error))


;;; multiple value
(define-library (picrin multiple-value)
  (import (scheme base)
          (scheme cxr)
          (picrin macro)
          (picrin core-syntax))

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
     (lambda (form r c)
       (let ((formals (cadr form)))
         `(,(r 'begin)
            ,@(do ((vars formals (cdr vars))
                   (defs '()))
                  ((null? vars)
                   defs)
                (set! defs (cons `(,(r 'define) ,(car vars) #f) defs)))
            (,(r 'call-with-values)
                (,(r 'lambda) () ,@(cddr form))
              (,(r 'lambda) (,@(map r formals))
                ,@(do ((vars formals (cdr vars))
                       (assn '()))
                      ((null? vars)
                       assn)
                    (set! assn (cons `(,(r 'set!) ,(car vars) ,(r (car vars))) assn))))))))))

  (export let-values
          let*-values
          define-values))

;;; parameter
(define-library (picrin parameter)
  (import (scheme base)
          (scheme cxr)
          (picrin macro)
          (picrin core-syntax))

  ;; reopen (pircin parameter)
  ;; see src/var.c

  (define-syntax parameterize
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (cadr form))
             (body (cddr form)))
         (let ((vars (map car bindings))
               (gensym (lambda (var)
                         (string->symbol
                          (string-append
                           "parameterize-"
                           (symbol->string var))))))
           `(,(r 'let) (,@(map (lambda (var)
                                 `(,(r (gensym var)) (,var)))
                            vars))
              ,@bindings
              (,(r 'let) ((,(r 'result) (begin ,@body)))
                ,@(map (lambda (var)
                         `(,(r 'parameter-set!) ,var ,(r (gensym var))))
                       vars)
                ,(r 'result))))))))

  (export parameterize))

;;; Record Type
(define-library (picrin record)
  (import (scheme base)
	  (scheme cxr)
	  (picrin macro)
          (picrin core-syntax))

  (define record-marker (list 'record-marker))

  (define real-vector? vector?)

  (define (vector? x)
    (and (real-vector? x)
	 (or (= 0 (vector-length x))
	     (not (eq? (vector-ref x 0)
		       record-marker)))))

  #|
  ;; (scheme eval) is not provided for now
  (define eval
    (let ((real-eval eval))
      (lambda (exp env)
	((real-eval `(lambda (vector?) ,exp))
	 vector?))))
  |#

  (define (record? x)
    (and (real-vector? x)
	 (< 0 (vector-length x))
	 (eq? (vector-ref x 0) record-marker)))

  (define (make-record size)
    (let ((new (make-vector (+ size 1))))
      (vector-set! new 0 record-marker)
      new))

  (define (record-ref record index)
    (vector-ref record (+ index 1)))

  (define (record-set! record index value)
    (vector-set! record (+ index 1) value))

  (define record-type% (make-record 3))
  (record-set! record-type% 0 record-type%)
  (record-set! record-type% 1 'record-type%)
  (record-set! record-type% 2 '(name field-tags))

  (define (make-record-type name field-tags)
    (let ((new (make-record 3)))
      (record-set! new 0 record-type%)
      (record-set! new 1 name)
      (record-set! new 2 field-tags)
      new))

  (define (record-type record)
    (record-ref record 0))

  (define (record-type-name record-type)
    (record-ref record-type 1))

  (define (record-type-field-tags record-type)
    (record-ref record-type 2))

  (define (field-index type tag)
    (let rec ((i 1) (tags (record-type-field-tags type)))
      (cond ((null? tags)
	     (error "record type has no such field" type tag))
	    ((eq? tag (car tags)) i)
	    (else (rec (+ i 1) (cdr tags))))))

  (define (record-constructor type tags)
    (let ((size (length (record-type-field-tags type)))
	  (arg-count (length tags))
	  (indexes (map (lambda (tag) (field-index type tag)) tags)))
      (lambda args
	(if (= (length args) arg-count)
	    (let ((new (make-record (+ size 1))))
	      (record-set! new 0 type)
	      (for-each (lambda (arg i) (record-set! new i arg)) args indexes)
	      new)
	    (error "wrong number of arguments to constructor" type args)))))

  (define (record-predicate type)
    (lambda (thing)
      (and (record? thing)
	   (eq? (record-type thing)
		type))))

  (define (record-accessor type tag)
    (let ((index (field-index type tag)))
      (lambda (thing)
	(if (and (record? thing)
		 (eq? (record-type thing)
		      type))
	    (record-ref thing index)
	    (error "accessor applied to bad value" type tag thing)))))

  (define (record-modifier type tag)
    (let ((index (field-index type tag)))
      (lambda (thing value)
	(if (and (record? thing)
		 (eq? (record-type thing)
		      type))
	    (record-set! thing index value)
	    (error "modifier applied to bad value" type tag thing)))))

  (define-syntax define-record-field
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((type (cadr form))
	     (field-tag (caddr form))
	     (acc-mod (cdddr form)))
	 (if (= 1 (length acc-mod))
	     `(define ,(car acc-mod)
		(record-accessor ,type ',field-tag))
	     `(begin
		(define ,(car acc-mod)
		  (record-accessor ,type ',field-tag))
		(define ,(cadr acc-mod)
		  (record-modifier ,type ',field-tag))))))))

  (define-syntax define-record-type
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((type (cadr form))
	     (constructor (caddr form))
	     (predicate (cadddr form))
	     (field-tag (cddddr form)))
	 `(begin
	    (define ,type
	      (make-record-type ',type ',(cdr constructor)))
	    (define ,(car constructor)
	      (record-constructor ,type ',(cdr constructor)))
	    (define ,predicate
	      (record-predicate ,type))
	    ,@(map
	       (lambda (x)
		 `(define-record-field ,type ,(car x) ,(cadr x) ,@(cddr x)))
	       field-tag))))))

  (export define-record-type vector?))

(import (picrin macro)
        (picrin core-syntax)
        (picrin multiple-value)
        (picrin parameter)
        (picrin record))

(export let let* letrec letrec*
        quasiquote unquote unquote-splicing
        and or
        cond case else =>
        do when unless
        _ ... syntax-error)

(export let-values
        let*-values
        define-values)

(export make-parameter
        parameterize)

(export vector?                         ; override definition
        define-record-type)

(define (every pred list)
  (if (null? list)
      #t
      (if (pred (car list))
	  (every pred (cdr list))
	  #f)))

(define (fold f s xs)
  (if (null? xs)
      s
      (fold f (f (car xs) s) (cdr xs))))

;;; 6.2. Numbers

(define (floor/ n m)
  (values (floor-quotient n m)
	  (floor-remainder n m)))

(define (truncate/ n m)
  (values (truncate-quotient n m)
	  (truncate-remainder n m)))

; (import (only (scheme inexact) sqrt))
(import (scheme inexact))

(define (exact-integer-sqrt k)
  (let ((n (exact (floor (sqrt k)))))
    (values n (- k (square n)))))

(export floor/ truncate/
        exact-integer-sqrt)

;;; 6.3 Booleans

(define (boolean=? . objs)
  (or (every (lambda (x) (eq? x #t)) objs)
      (every (lambda (x) (eq? x #f)) objs)))

(export boolean=?)

;;; 6.4 Pairs and lists

(define (memq obj list)
  (if (null? list)
      #f
      (if (eq? obj (car list))
	  list
	  (memq obj (cdr list)))))

(define (memv obj list)
  (if (null? list)
      #f
      (if (eqv? obj (car list))
	  list
	  (memq obj (cdr list)))))

(define (assq obj list)
  (if (null? list)
      #f
      (if (eq? obj (caar list))
	  (car list)
	  (assq obj (cdr list)))))

(define (assv obj list)
  (if (null? list)
      #f
      (if (eqv? obj (caar list))
	  (car list)
	  (assq obj (cdr list)))))

(define (member obj list . opts)
  (let ((compare (if (null? opts) equal? (car opts))))
    (if (null? list)
	#f
	(if (compare obj (car list))
	    list
	    (member obj (cdr list) compare)))))

(define (assoc obj list . opts)
  (let ((compare (if (null? opts) equal? (car opts))))
    (if (null? list)
	#f
	(if (compare obj (caar list))
	    (car list)
	    (assoc obj (cdr list) compare)))))

(export memq memv member
        assq assv assoc)

;;; 6.5. Symbols

(define (symbol=? . objs)
  (let ((sym (car objs)))
    (if (symbol? sym)
	(every (lambda (x)
		 (and (symbol? x)
		      (eq? x sym)))
	       (cdr objs))
	#f)))

(export symbol=?)

;;; 6.6 Characters

(define-macro (define-char-transitive-predicate name op)
  `(define (,name . cs)
     (apply ,op (map char->integer cs))))

(define-char-transitive-predicate char=? =)
(define-char-transitive-predicate char<? <)
(define-char-transitive-predicate char>? >)
(define-char-transitive-predicate char<=? <=)
(define-char-transitive-predicate char>=? >=)

(export char=?
        char<?
        char>?
        char<=?
        char>=?)

;;; 6.7 String

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

(export string string->list list->string)

;;; 6.8. Vector

(define (vector . objs)
  (let ((len (length objs)))
    (let ((v (make-vector len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((= i len)
	   v)
	(vector-set! v i (car l))))))

(define (vector->list vector . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (vector-length vector))))
    (do ((i start (+ i 1))
	 (res '()))
	((= i end)
	 (reverse res))
      (set! res (cons (vector-ref vector i) res)))))

(define (list->vector list)
  (apply vector list))

(define (vector-copy! to at from . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (vector-length from))))
    (do ((i at (+ i 1))
	 (j start (+ j 1)))
	((= j end))
      (vector-set! to i (vector-ref from j)))))

(define (vector-copy v . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (vector-length v))))
    (let ((res (make-vector (vector-length v))))
      (vector-copy! res 0 v start end)
      res)))

(define (vector-append . vs)
  (define (vector-append-2-inv w v)
    (let ((res (make-vector (+ (vector-length v) (vector-length w)))))
      (vector-copy! res 0 v)
      (vector-copy! res (vector-length v) w)
      res))
  (fold vector-append-2-inv #() vs))

(define (vector-fill! v fill . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (vector-length v))))
    (do ((i start (+ i 1)))
	((= i end)
	 #f)
      (vector-set! v i fill))))

(define (vector->string . args)
  (list->string (apply vector->list args)))

(define (string->vector . args)
  (list->vector (apply string->list args)))

(export vector vector->list list->vector
        vector-copy! vector-copy
        vector-append vector-fill!
        vector->string string->vector)

;;; 6.9 bytevector

(define (bytevector . objs)
  (let ((len (length objs)))
    (let ((v (make-bytevector len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((= i len)
	   v)
	(bytevector-u8-set! v i (car l))))))

(define (bytevector-copy! to at from . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (bytevector-length from))))
    (do ((i at (+ i 1))
	 (j start (+ j 1)))
	((= j end))
      (bytevector-u8-set! to i (bytevector-u8-ref from j)))))

(define (bytevector-copy v . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (bytevector-length v))))
    (let ((res (make-bytevector (bytevector-length v))))
      (bytevector-copy! res 0 v start end)
      res)))

(define (bytevector-append . vs)
  (define (bytevector-append-2-inv w v)
    (let ((res (make-bytevector (+ (bytevector-length v) (bytevector-length w)))))
      (bytevector-copy! res 0 v)
      (bytevector-copy! res (bytevector-length v) w)
      res))
  (fold bytevector-append-2-inv #() vs))

(define (bytevector->list v start end)
    (do ((i start (+ i 1))
	 (res '()))
	((= i end)
	 (reverse res))
      (set! res (cons (bytevector-u8-ref v i) res))))

(define (list->bytevector v)
  (apply bytevector v))

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
        bytevector-copy!
        bytevector-copy
        bytevector-append
        utf8->string
        string->utf8)

;;; 6.10 control features

(define (string-map f v . vs)
  (let* ((len (fold min (string-length v) (map string-length vs)))
	 (vec (make-string len)))
    (let loop ((n 0))
      (if (= n len)
	  vec
	  (begin (string-set! vec n
			      (apply f (cons (string-ref v n)
					     (map (lambda (v) (string-ref v n)) vs))))
		 (loop (+ n 1)))))))

(define (string-for-each f v . vs)
  (let* ((len (fold min (string-length v) (map string-length vs))))
    (let loop ((n 0))
      (unless (= n len)
	(apply f (string-ref v n)
	       (map (lambda (v) (string-ref v n)) vs))
	(loop (+ n 1))))))

(define (vector-map f v . vs)
  (let* ((len (fold min (vector-length v) (map vector-length vs)))
	 (vec (make-vector len)))
    (let loop ((n 0))
      (if (= n len)
	  vec
	  (begin (vector-set! vec n
			      (apply f (cons (vector-ref v n)
					     (map (lambda (v) (vector-ref v n)) vs))))
		 (loop (+ n 1)))))))

(define (vector-for-each f v . vs)
  (let* ((len (fold min (vector-length v) (map vector-length vs))))
    (let loop ((n 0))
      (unless (= n len)
	(apply f (vector-ref v n)
	       (map (lambda (v) (vector-ref v n)) vs))
	(loop (+ n 1))))))

(export string-map string-for-each
        vector-map vector-for-each)

;;; 6.13. Input and output

(define (call-with-port port proc)
  (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close-port port))))

(export call-with-port)

(define-library (scheme write)
  (import (scheme base))

  (define (display obj . opts)
    (let ((port (if (null? opts) (current-output-port) (car opts))))
      (cond
       ((string? obj)
        (write-string obj port))
       ((char? obj)
        (write-char obj port))
       ((symbol? obj)
        (write-string (symbol->string obj) port))
       (else
        (write obj port)))))

  (export display))
