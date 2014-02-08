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

;;; bootstrap utilities
(define-library (picrin bootstrap-tools)
  (import (scheme base))

  (define (cadar p) (car (cdar p)))
  (define (caddr p) (car (cddr p)))
  (define (cdddr p) (cdr (cddr p)))

  (export cadar caddr cdddr))

;;; core syntaces
(define-library (picrin core-syntax)
  (import (scheme base)
          (picrin macro)
          (picrin bootstrap-tools))

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

  (define-syntax and
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((exprs (cdr expr)))
         (if (null? exprs)
             #t
             (list (r 'if) (car exprs)
                   (cons (r 'and) (cdr exprs))
                   #f))))))

  (define-syntax or
    (er-macro-transformer
     (lambda (expr r compare)
       (let ((exprs (cdr expr)))
         (if (null? exprs)
             #f
             (list (r 'let) (list (list (r 'it) (car exprs)))
                   (list (r 'if) (r 'it)
                         (r 'it)
                         (cons (r 'or) (cdr exprs)))))))))

  (define-syntax quasiquote
    (er-macro-transformer
     (lambda (expr r compare?)
       (let ((x (cadr expr)))
         (cond
          ((symbol? x) (list (r 'quote) x))
          ((pair? x) (cond
                      ((compare? (r 'unquote) (car x))
                       (cadr x))
                      ((and (pair? (car x))
                            (compare? (r 'unquote-splicing) (caar x)))
                       (list (r 'append) (cadar x)
                             (list (r 'quasiquote) (cdr x))))
                      (#t
                       (list (r 'cons)
                             (list (r 'quasiquote) (car x))
                             (list (r 'quasiquote) (cdr x))))))
          (#t x))))))

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
          (picrin macro)
          (picrin core-syntax)
          (picrin bootstrap-tools))

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
          (picrin macro)
          (picrin core-syntax)
          (picrin bootstrap-tools))

  ;; reopen (pircin parameter)
  ;; see src/var.c

  (define-syntax parameterize
    (er-macro-transformer
     (lambda (form r compare)
       (let ((bindings (cadr form))
             (body (cddr form)))
         (let ((vars (map car bindings)))
           `(,(r 'let) (,@(map (lambda (var)
                                 `(,(r var) (,var)))
                            vars))
              ,@bindings
              (,(r 'let) ((,(r 'result) (begin ,@body)))
                ,@(map (lambda (var)
                         `(,(r 'parameter-set!) ,var ,(r var)))
                       vars)
                ,(r 'result))))))))

  (export parameterize))

(import (picrin macro)
        (picrin core-syntax)
        (picrin multiple-value)
        (picrin parameter))

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

(define (string . objs)
  (let ((len (length objs)))
    (let ((v (make-string len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((= i len)
	   v)
	(string-set! v i (car l))))))

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
  (apply string list))

(define (string-copy! to at from . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (string-length from))))
    (do ((i at (+ i 1))
	 (j start (+ j 1)))
	((= j end))
      (string-set! to i (string-ref from j)))))

(define (string-copy v . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (string-length v))))
    (let ((res (make-string (string-length v))))
      (string-copy! res 0 v start end)
      res)))

(define (string-append . vs)
  (define (string-append-2-inv w v)
    (let ((res (make-string (+ (string-length v) (string-length w)))))
      (string-copy! res 0 v)
      (string-copy! res (string-length v) w)
      res))
  (fold string-append-2-inv #() vs))

(define (string-fill! v fill . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (string-length v))))
    (do ((i start (+ i 1)))
	((= i end)
	 #f)
      (string-set! v i fill))))

(export string string->list list->string
        string-copy! string-copy
        string-append string-fill!)

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

;;; Appendix A. Standard Libraries
;; CxR
(define-library (scheme cxr)
  (import (scheme base))

  (define (caaar  p) (car  (caar p)))
  (define (caadr  p) (car  (cadr p)))
  (define (cadar  p) (car  (cdar p)))
  (define (caddr  p) (car  (cddr p)))
  (define (cdaar  p) (cdr  (caar p)))
  (define (cdadr  p) (cdr  (cadr p)))
  (define (cddar  p) (cdr  (cdar p)))
  (define (cdddr  p) (cdr  (cddr p)))
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

  (export caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
