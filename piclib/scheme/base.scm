(define-library (scheme base)
  (import (picrin base)
          (picrin macro))

  (export define
          set!
          lambda
          quote
          if
          begin
          define-syntax)

  ;; core syntax

  (import (scheme file))

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
          _ ... syntax-error)


  ;; utility functions

  (define (walk proc expr)
    (cond
     ((null? expr)
      '())
     ((pair? expr)
      (cons (walk proc (car expr))
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

  (define (reverse* l)
    ;; (reverse* '(a b c d . e)) => (e d c b a)
    (let loop ((a '())
	       (d l))
      (if (pair? d)
	  (loop (cons (car d) a) (cdr d))
	  (cons d a))))

  (define (every? pred l)
    (if (null? l)
	#t
	(and (pred (car l)) (every? pred (cdr l)))))


  ;; extra syntax

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
          define-values)

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
       (define _vector? (r 'vector?))
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

       (define (var->sym v)
         (let loop ((cnt 0)
                    (v v))
           (if (symbol? v)
               (string->symbol
                (string-append (symbol->string v) "/" (number->string cnt)))
               (loop (+ 1 cnt) (car v)))))

       (define push-var list)

       (define (compile-match ellipsis literals pattern)
	 (letrec ((compile-match-base
		   (lambda (pattern)
		     (cond ((member pattern literals compare)
			    (values
			     `(,_if (,_and (,_symbol? expr) (cmp expr (rename ',pattern)))
				    #f
				    (exit #f))
			     '()))
			   ((compare pattern (r '_)) (values #f '()))
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
		     (let-values (((match vars) (compile-match-base (vector->list pattern))))
		       (values
			`(,_if (,_vector? expr)
			       (,_let ((expr (,_vector->list expr)))
				      ,match)
			       (exit #f))
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
				   (compile-expand-base (vector->list template) ellipsis-valid)))
		       (values
			`(,_list->vector ,expand1)
			vars1)))))

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

  (export syntax-rules)


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

  (export parameterize make-parameter)


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

  ;; 5.5 Recored-type definitions

  (import (picrin record))

  (export define-record-type)

  (export (rename floor-remainder modulo)
          (rename truncate-quotient quotient)
          (rename truncate-remainder remainder))

  (export define
          lambda
          if
          quote
          set!
          begin
          define-syntax)

  (export eq?
          eqv?
          equal?)

  (export boolean?
          boolean=?
          not)

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
          exact-integer?
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
          make-string
          string-length
          string-ref
          string-set!
          string=?
          string<?
          string>?
          string<=?
          string>=?
          string-copy
          string-copy!
          string-append
          string-fill!)

  (export current-input-port
          current-output-port
          current-error-port

          port?
          input-port?
          output-port?
          textual-port?
          binary-port?
          close-port

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
          for-each)

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

  ;; 6.5 Symbols

  (export symbol?
          symbol=?
          symbol->string
          string->symbol)

  ;; 6.6 Characters

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

  ;; 6.7 String

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

  (export string
          string->list
          list->string
          (rename string-copy substring))

  ;; 6.8. Vector

  (define (vector . objs)
    (list->vector objs))

  (define (vector->string . args)
    (list->string (apply vector->list args)))

  (define (string->vector . args)
    (list->vector (apply string->list args)))

  (export vector vector->string string->vector)

  ;; 6.9 bytevector

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

  ;; 6.10 control features

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

  ;; 6.13. Input and output

  (define (call-with-port port proc)
    (dynamic-wind
        (lambda () #f)
        (lambda () (proc port))
        (lambda () (close-port port))))

  (export call-with-port))
