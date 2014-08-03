;;; core syntaces
(define-library (picrin core-syntax)
  (import (scheme base)
          (picrin macro))

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
    (ir-macro-transformer
     (lambda (form inject compare)

       (define (quasiquote? form)
         (and (pair? form) (compare (car form) 'quasiquote)))

       (define (unquote? form)
         (and (pair? form) (compare (car form) 'unquote)))

       (define (unquote-splicing? form)
         (and (pair? form) (pair? (car form))
              (compare (car (car form)) 'unquote-splicing)))

       (define (qq depth expr)
         (cond
          ;; unquote
          ((unquote? expr)
           (if (= depth 1)
               (car (cdr expr))
               (list 'list
                     (list 'quote (inject 'unquote))
                     (qq (- depth 1) (car (cdr expr))))))
          ;; unquote-splicing
          ((unquote-splicing? expr)
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
          ((quasiquote? expr)
           (list 'list
                 (list 'quote (inject 'quasiquote))
                 (qq (+ depth 1) (car (cdr expr)))))
          ;; list
          ((pair? expr)
           (list 'cons
                 (qq depth (car expr))
                 (qq depth (cdr expr))))
          ;; vector
          ((vector? expr)
           (list 'list->vector (qq depth (vector->list expr))))
          ;; simple datum
          (else
           (list 'quote expr))))

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

  (import (scheme read) (scheme file))

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

  (export let let* letrec letrec*
          quasiquote unquote unquote-splicing
          and or
          cond case else =>
          do when unless
          let-syntax letrec-syntax
          include
          _ ... syntax-error))

(import (picrin core-syntax))

(export let let* letrec letrec*
        quasiquote unquote unquote-splicing
        and or
        cond case else =>
        do when unless
        let-syntax letrec-syntax
        include
        _ ... syntax-error)

;;; multiple value
(define-library (picrin values)
  (import (scheme base)
          (picrin macro))

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

  (define (walk proc expr)
    (cond
     ((null? expr)
      '())
     ((pair? expr)
      (cons (proc (car expr))
            (walk proc (cdr expr))))
     ((vector? expr)
      (list->vector (map proc (vector->list expr))))
     (else
      (proc expr))))

  (define (flatten expr)
    (let ((list '()))
      (walk
       (lambda (x)
         (set! list (cons x list)))
       expr)
      (reverse list)))

  (define uniq
    (let ((counter 0))
      (lambda (x)
        (let ((sym (string->symbol (string-append "var$" (number->string counter)))))
          (set! counter (+ counter 1))
          sym))))

  (define-syntax define-values
    (ir-macro-transformer
     (lambda (form inject compare)
       (let* ((formal  (cadr form))
              (formal* (walk uniq formal))
              (exprs   (cddr form)))
         `(begin
            ,@(map
               (lambda (var) `(define ,var #f))
               (flatten formal))
            (call-with-values (lambda () ,@exprs)
              (lambda ,formal*
                ,@(map
                   (lambda (var val) `(set! ,var ,val))
                   (flatten formal)
                   (flatten formal*)))))))))

  (export let-values
          let*-values
          define-values))

;;; parameter
(define-library (picrin parameter)
  (import (scheme base)
          (picrin macro))

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

  (export parameterize))

;;; Record Type
(define-library (picrin record)
  (import (scheme base)
	  (picrin macro)
	  (picrin record-primitive))

  (define (caddr x) (car (cddr x)))
  (define (cdddr x) (cdr (cddr x)))
  (define (cadddr x) (car (cdddr x)))
  (define (cddddr x) (cdr (cdddr x)))

  (define (make-record-type name)
    (let ((rectype (make-record #t)))
      (record-set! rectype #t 'name name)
      rectype))

  (define-syntax define-record-constructor
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((rectype (cadr form))
	     (name  (caddr form))
	     (fields (cdddr form)))
	 `(define (,name ,@fields)
	    (let ((record (make-record ,rectype)))
	      ,@(map (lambda (field)
		       `(record-set! record ,rectype ',field ,field))
		     fields)
	      record))))))

  (define-syntax define-record-predicate
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((rectype (cadr form))
	     (name (caddr form)))
	 `(define (,name obj)
	    (record-of? obj ,rectype))))))

  (define-syntax define-record-field
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((rectype (cadr form))
	     (field-name (caddr form))
	     (accessor (cadddr form))
	     (modifier? (cddddr form)))
	 (if (null? modifier?)
	     `(define (,accessor record)
		(record-roef record ,rectype ',field-name))
	     `(begin
		(define (,accessor record)
		  (record-ref record ,rectype ',field-name))
		(define (,(car modifier?) record val)
		  (record-set! record ,rectype ',field-name val))))))))

  (define-syntax define-record-type
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((name (cadr form))
	     (constructor (caddr form))
	     (pred (cadddr form))
	     (fields (cddddr form)))
	 `(begin
	    (define ,name (make-record-type ',name))
	    (define-record-constructor ,name ,@constructor)
	    (define-record-predicate ,name ,pred)
	    ,@(map (lambda (field) `(define-record-field ,name ,@field))
		   fields))))))

  (export define-record-type))

(import (picrin macro)
        (picrin values)
        (picrin parameter)
	(picrin record))

(export let-values
        let*-values
        define-values)

(export make-parameter
        parameterize)

(export define-record-type)

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
  (list->vector objs))

(define (vector->string . args)
  (list->string (apply vector->list args)))

(define (string->vector . args)
  (list->vector (apply string->list args)))

(export vector vector->string string->vector)

;;; 6.9 bytevector

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

;;; 6.10 control features

(define (string-map f . strings)
  (list->string (apply map f (map string->list strings))))

(define (string-for-each f . strings)
  (apply for-each f (map string->list strings)))

(define (vector-map f . vectors)
  (list->vector (apply map f (map vector->list vectors))))

(define (vector-for-each f . vectors)
  (apply for-each f (map vector->list vectors)))

(export string-map string-for-each
        vector-map vector-for-each)

;;; 6.13. Input and output

(define (call-with-port port proc)
  (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close-port port))))

(export call-with-port)

;;; syntax-rules
(define-library (picrin syntax-rules)
  (import (scheme base)
          (picrin macro))

  ;;; utility functions
  (define (reverse* l)
    ;; (reverse* '(a b c d . e)) => (e d c b a)
    (let loop ((a '())
	       (d l))
      (if (pair? d)
	  (loop (cons (car d) a) (cdr d))
	  (cons d a))))

  (define (var->sym v)
    (let loop ((cnt 0)
	       (v v))
      (if (symbol? v)
	  (string->symbol (string-append (symbol->string v) "/" (number->string cnt)))
	  (loop (+ 1 cnt) (car v)))))

  (define push-var list)

  (define (every? pred l)
    (if (null? l)
	#t
	(and (pred (car l)) (every? pred (cdr l)))))

  (define (flatten l)
    (cond
     ((null? l) '())
     ((pair? (car l))
      (append (flatten (car l)) (flatten (cdr l))))
     (else
      (cons (car l) (flatten (cdr l))))))

  ;;; main function
  (define-syntax syntax-rules
    (er-macro-transformer
     (lambda (form r compare)
       (define _define (r 'define))
       (define _let (r 'let))
       (define _if (r 'if))
       (define _begin (r 'begin))
       (define _lambda (r 'lambda))
       (define _set! (r 'set!))
       (define _not (r 'not))
       (define _and (r 'and))
       (define _car (r 'car))
       (define _cdr (r 'cdr))
       (define _cons (r 'cons))
       (define _pair? (r 'pair?))
       (define _null? (r 'null?))
       (define _symbol? (r 'symbol?))
       (define _eqv? (r 'eqv?))
       (define _string=? (r 'string=?))
       (define _map (r 'map))
       (define _vector->list (r 'vector->list))
       (define _list->vector (r 'list->vector))
       (define _quote (r 'quote))
       (define _quasiquote (r 'quasiquote))
       (define _unquote (r 'unquote))
       (define _unquote-splicing (r 'unquote-splicing))
       (define _syntax-error (r 'syntax-error))
       (define _call/cc (r 'call/cc))
       (define _er-macro-transformer (r 'er-macro-transformer))

       (define (compile-match ellipsis literals pattern)
	 (letrec ((compile-match-base
		   (lambda (pattern)
		     (cond ((compare pattern (r '_)) (values #f '()))
			   ((member pattern literals compare)
			    (values
			     `(,_if (,_and (,_symbol? expr) (cmp expr (rename ',pattern)))
				    #f
				    (exit #f))
			     '()))
			   ((and ellipsis (compare pattern ellipsis))
			    (values `(,_syntax-error "invalid pattern") '()))
			   ((symbol? pattern)
			    (values `(,_set! ,(var->sym pattern) expr) (list pattern)))
			   ((pair? pattern)
			    (compile-match-list pattern))
			   ((vector? pattern)
			    (compile-match-vector pattern))
			   ((string? pattern)
			    (values
			     `(,_if (,_not (,_string=? ',pattern expr))
				    (exit #f))
			     '()))
			   (else
			    (values
			     `(,_if (,_not (,_eqv? ',pattern expr))
				    (exit #f))
			     '())))))

		  (compile-match-list
		   (lambda (pattern)
		     (let loop ((pattern pattern)
				(matches '())
				(vars '())
				(accessor 'expr))
		       (cond ;; (hoge)
			((not (pair? (cdr pattern)))
			 (let*-values (((match1 vars1) (compile-match-base (car pattern)))
				       ((match2 vars2) (compile-match-base (cdr pattern))))
			   (values
			    `(,_begin ,@(reverse matches)
				      (,_if (,_pair? ,accessor)
					    (,_begin
					     (,_let ((expr (,_car ,accessor)))
						    ,match1)
					     (,_let ((expr (,_cdr ,accessor)))
						    ,match2))
					    (exit #f)))
			    (append vars (append vars1 vars2)))))
			;; (hoge ... rest args)
			((and ellipsis (compare (cadr pattern) ellipsis))
			 (let-values (((match-r vars-r) (compile-match-list-reverse pattern)))
			   (values
			    `(,_begin ,@(reverse matches)
				      (,_let ((expr (,_let loop ((a ())
								 (d ,accessor))
							   (,_if (,_pair? d)
								 (loop (,_cons (,_car d) a) (,_cdr d))
								 (,_cons d a)))))
					     ,match-r))
			    (append vars vars-r))))
			(else
			 (let-values (((match1 vars1) (compile-match-base (car pattern))))
			   (loop (cdr pattern)
				 (cons `(,_if (,_pair? ,accessor)
					      (,_let ((expr (,_car ,accessor)))
						     ,match1)
					      (exit #f))
				       matches)
				 (append vars vars1)
				 `(,_cdr ,accessor))))))))

		  (compile-match-list-reverse
		   (lambda (pattern)
		     (let loop ((pattern (reverse* pattern))
				(matches '())
				(vars '())
				(accessor 'expr))
		       (cond ((and ellipsis (compare (car pattern) ellipsis))
			      (let-values (((match1 vars1) (compile-match-ellipsis (cadr pattern))))
				(values
				 `(,_begin ,@(reverse matches)
					   (,_let ((expr ,accessor))
						  ,match1))
				 (append vars vars1))))
			     (else
			      (let-values (((match1 vars1) (compile-match-base (car pattern))))
				(loop (cdr pattern)
				      (cons `(,_let ((expr (,_car ,accessor))) ,match1) matches)
				      (append vars vars1)
				      `(,_cdr ,accessor))))))))

		  (compile-match-ellipsis
		   (lambda (pattern)
		     (let-values (((match vars) (compile-match-base pattern)))
		       (values
			`(,_let loop ((expr expr))
				(,_if (,_not (,_null? expr))
				      (,_let ,(map (lambda (var) `(,(var->sym var) '())) vars)
					     (,_let ((expr (,_car expr)))
						    ,match)
					     ,@(map
						(lambda (var)
						  `(,_set! ,(var->sym (push-var var))
							   (,_cons ,(var->sym var) ,(var->sym (push-var var)))))
						vars)
					     (loop (,_cdr expr)))))
			(map push-var vars)))))

		  (compile-match-vector
		   (lambda (pattern)
		     (let-values (((match vars) (compile-match-list (vector->list pattern))))
		       (values
			`(,_let ((expr (,_vector->list expr)))
				,match)
			vars)))))

	   (let-values (((match vars) (compile-match-base (cdr pattern))))
	     (values `(,_let ((expr (,_cdr expr)))
			     ,match
			     #t)
		     vars))))

       ;;; compile expand
       (define (compile-expand ellipsis reserved template)
	 (letrec ((compile-expand-base
		   (lambda (template ellipsis-valid)
		     (cond ((member template reserved eq?)
			    (values (var->sym template) (list template)))
			   ((symbol? template)
			    (values `(rename ',template) '()))
			   ((pair? template)
			    (compile-expand-list template ellipsis-valid))
			   ((vector? template)
			    (compile-expand-vector template ellipsis-valid))
			   (else
			    (values `',template '())))))

		  (compile-expand-list
		   (lambda (template ellipsis-valid)
		     (let loop ((template template)
				(expands '())
				(vars '()))
		       (cond ;; (... hoge)
			((and ellipsis-valid
			      (pair? template)
			      (compare (car template) ellipsis))
			 (if (and (pair? (cdr template)) (null? (cddr template)))
			     (compile-expand-base (cadr template) #f)
			     (values '(,_syntax-error "invalid template") '())))
			;; hoge
			((not (pair? template))
			 (let-values (((expand1 vars1)
				       (compile-expand-base template ellipsis-valid)))
			   (values
			    `(,_quasiquote (,@(reverse expands) . (,_unquote ,expand1)))
			    (append vars vars1))))
			;; (a ... rest syms)
			((and ellipsis-valid
			      (pair? (cdr template))
			      (compare (cadr template) ellipsis))
			 (let-values (((expand1 vars1)
				       (compile-expand-base (car template) ellipsis-valid)))
			   (loop (cddr template)
				 (cons
				  `(,_unquote-splicing
				    (,_map (,_lambda ,(map var->sym vars1) ,expand1)
					   ,@(map (lambda (v) (var->sym (push-var v))) vars1)))
				  expands)
				 (append vars (map push-var vars1)))))
			(else
			 (let-values (((expand1 vars1)
				       (compile-expand-base (car template) ellipsis-valid)))
			   (loop (cdr template)
				 (cons
				  `(,_unquote ,expand1)
				  expands)
				 (append vars vars1))))))))

		  (compile-expand-vector
		   (lambda (template ellipsis-valid)
		     (let-values (((expand1 vars1)
				   (compile-expand-list (vector->list template) ellipsis-valid)))
		       `(,_list->vector ,expand1)
		       vars1))))

	   (compile-expand-base template ellipsis)))

       (define (check-vars vars-pattern vars-template)
	 ;;fixme
	 #t)

       (define (compile-rule ellipsis literals rule)
	 (let ((pattern (car rule))
	       (template (cadr rule)))
	   (let*-values (((match vars-match)
			  (compile-match ellipsis literals pattern))
			 ((expand vars-expand)
			  (compile-expand ellipsis (flatten vars-match) template)))
	     (if (check-vars vars-match vars-expand)
		 (list vars-match match expand)
		 'mismatch))))

       (define (expand-clauses clauses rename)
	 (cond ((null? clauses)
		`(,_quote (syntax-error "no matching pattern")))
	       ((compare (car clauses) 'mismatch)
		`(,_syntax-error "invalid rule"))
	       (else
		(let ((vars (list-ref (car clauses) 0))
		      (match (list-ref (car clauses) 1))
		      (expand (list-ref (car clauses) 2)))
		  `(,_let ,(map (lambda (v) (list (var->sym v) '())) vars)
			  (,_let ((result (,_call/cc (,_lambda (exit) ,match))))
				 (,_if result
				       ,expand
				       ,(expand-clauses (cdr clauses) rename))))))))

       (define (normalize-form form)
	 (if (and (list? form) (>= (length form) 2))
	     (let ((ellipsis '...)
		   (literals (cadr form))
		   (rules (cddr form)))

	       (when (symbol? literals)
		     (set! ellipsis literals)
		     (set! literals (car rules))
		     (set! rules (cdr rules)))

	       (if (and (symbol? ellipsis)
			(list? literals)
			(every? symbol? literals)
			(list? rules)
			(every? (lambda (l) (and (list? l) (= (length l) 2))) rules))
		   (if (member ellipsis literals compare)
		       `(syntax-rules #f ,literals ,@rules)
		       `(syntax-rules ,ellipsis ,literals ,@rules))
		   #f))
	     #f))

       (let ((form (normalize-form form)))
	 (if form
	     (let ((ellipsis (list-ref form 1))
		   (literals (list-ref form 2))
		   (rules (list-tail form 3)))
	       (let ((clauses (map (lambda (rule) (compile-rule ellipsis literals rule))
				   rules)))
		 `(,_er-macro-transformer
		   (,_lambda (expr rename cmp)
			     ,(expand-clauses clauses r)))))

	     `(,_syntax-error "malformed syntax-rules"))))))

  (export syntax-rules))

(import (picrin syntax-rules))
(export syntax-rules)

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

