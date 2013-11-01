; Although looking like a magic, it just works.
(define (car x)
  (car x))

(define (cdr x)
  (cdr x))

(define (zero? n)
  (= n 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (caar p)
  (car (car p)))

(define (cadr p)
  (car (cdr p)))

(define (cdar p)
  (cdr (car p)))

(define (cddr p)
  (cdr (cdr p)))

(define (list . args)
  args)

(define (list? obj)
  (if (null? obj)
      #t
      (if (pair? obj)
	  (list? (cdr obj))
	  #f)))

(define (make-list k . args)
  (if (null? args)
      (make-list k #f)
      (if (zero? k)
	  '()
	  (cons (car args)
		(make-list (- k 1) (car args))))))

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs)
	    (append (cdr xs) ys))))

(define (reverse list . args)
  (if (null? args)
      (reverse list '())
      (if (null? list)
	  (car args)
	  (reverse (cdr list)
		   (cons (car list) (car args))))))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (car (list-tail list k)))

(define (list-set! list k obj)
  (set-car! (list-tail list k) obj))

(define (list-copy obj)
  (if (null? obj)
      obj
      (cons (car obj)
	    (list-copy (cdr obj)))))

(define (map f list)
  (if (null? list)
      '()
      (cons (f (car list))
	    (map f (cdr list)))))

(define-macro (let bindings . body)
  (cons (cons 'lambda (cons (map car bindings) body))
	(map cadr bindings)))

(define-macro (cond . clauses)
  (if (null? clauses)
      #f
      (let ((c (car clauses)))
	(let ((test (car c))
	      (if-true (cons 'begin (cdr c)))
	      (if-false (cons 'cond (cdr clauses))))
	  (list 'if test if-true if-false)))))

(define-macro (and . exprs)
  (if (null? exprs)
      #t
      (let ((test (car exprs))
	    (if-true (cons 'and (cdr exprs))))
	(list 'if test if-true #f))))

(define-macro (or . exprs)
  (if (null? exprs)
      #f
      (let ((test (car exprs))
	    (if-false (cons 'or (cdr exprs))))
	(list 'let (list (list 'it test))
	      (list 'if 'it 'it if-false)))))

(define-macro (quasiquote x)
  (cond
   ((symbol? x) (list 'quote x))
   ((pair? x)
    (cond
     ((eq? 'unquote (car x)) (cadr x))
     ((and (pair? (car x))
	   (eq? 'unquote-splicing (caar x)))
      (list 'append (cadr (car x)) (list 'quasiquote (cdr x))))
     (#t (list 'cons
	       (list 'quasiquote (car x))
	       (list 'quasiquote (cdr x))))))
   (#t x)))
