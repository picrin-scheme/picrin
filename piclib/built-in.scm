(define (zero? n)
  (= n 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (odd? n)
  (= 0 (floor-remainder n 2)))

(define (even? n)
  (= 1 (floor-remainder n 2)))

(define (gcd n m)
  (if (negative? n)
      (set! n (- n)))
  (if (negative? m)
      (set! m (- m)))
  (if (> n m)
      ((lambda (tmp)
	 (set! n m)
	 (set! m tmp))
       n))
  (if (zero? n)
      m
      (gcd (floor-remainder m n) n)))

(define (lcm n m)
  (/ (* n m) (gcd n m)))

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

(define (list-copy obj)
  (if (null? obj)
      obj
      (cons (car obj)
	    (list-copy (cdr obj)))))

(define (every pred list)
  (if (null? list)
      #t
      (if (pred (car list))
	  (every pred (cdr list))
	  #f)))

(define (any pred list)
  (if (null? list)
      #f
      ((lambda (it)
	 (if it
	     it
	     (any pred (cdr list))))
       (pred (car list)))))

(define (fold f s xs)
  (if (null? xs)
      s
      (fold f (f (car xs) s) (cdr xs))))

(define (map f list . lists)
  (define (single-map f list)
    (if (null? list)
	'()
	(cons (f (car list))
	      (map f (cdr list)))))
  (define (multiple-map f lists)
    (if (any null? lists)
	'()
	(cons (apply f (single-map car lists))
	      (multiple-map f (single-map cdr lists)))))
  (if (null? lists)
      (single-map f list)
      (multiple-map f (cons list lists))))

(define (for-each f list . lists)
  (define (single-for-each f list)
    (if (null? list)
	#f
	(begin
	  (f (car list))
	  (single-for-each f (cdr list)))))
  (define (multiple-for-each f lists)
    (if (any null? lists)
	#f
	(begin
	  (apply f (map car lists))
	  (multiple-for-each f (map cdr lists)))))
  (if (null? lists)
      (single-for-each f list)
      (multiple-for-each f (cons list lists))))

(define-macro (let bindings . body)
  (if (symbol? bindings)
      (begin
	(define name bindings)
	(set! bindings (car body))
	(set! body (cdr body))
	;; expanded form should be like below:
	;; `(let ()
	;;    (define ,loop
	;;      (lambda (,@vars)
	;;        ,@body))
	;;    (,loop ,@vals))
	(list 'let '()
	      (list 'define name
		    (cons 'lambda (cons (map car bindings) body)))
	      (cons name (map cadr bindings))))
      (cons (cons 'lambda (cons (map car bindings) body))
	    (map cadr bindings))))

(define-macro (cond . clauses)
  (if (null? clauses)
      #f
      (let ((c (car clauses)))
	(let ((test (car c))
	      (if-true (cons 'begin (cdr c)))
	      (if-false (cons 'cond (cdr clauses))))
	  (list 'if test if-true if-false)))))

(define else #t)

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

(define-macro (let* bindings . body)
  (if (null? bindings)
      `(let () ,@body)
      `(let ((,(caar bindings)
	      ,@(cdar bindings)))
	 (let* (,@(cdr bindings))
	   ,@body))))

(define-macro (letrec bindings . body)
  (let ((vars (map (lambda (v) `(,v #f)) (map car bindings)))
	(initials (map (lambda (v) `(set! ,@v)) bindings)))
    `(let (,@vars)
       (begin ,@initials)
       ,@body)))

(define-macro (letrec* . args)
  `(letrec ,@args))

(define-macro (when test . exprs)
  (list 'if test (cons 'begin exprs) #f))

(define-macro (unless test . exprs)
  (list 'if test #f (cons 'begin exprs)))

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

(define (values . args)
  (if (and (pair? args)
	   (null? (cdr args)))
      (car args)
      (cons '*values-tag* args)))

(define (call-with-values producer consumer)
  (let ((res (producer)))
    (if (and (pair? res)
	     (eq? '*values-tag* (car res)))
        (apply consumer (cdr res))
        (consumer res))))

(define-macro (do bindings finish . body)
  `(let loop ,(map (lambda (x)
		     (list (car x) (cadr x)))
		   bindings)
     (if ,(car finish)
	 (begin ,@body
		(loop ,@(map (lambda (x)
			       (if (null? (cddr x))
				   (car x)
				   (car (cddr x))))
			     bindings)))
	 (begin ,@(cdr finish)))))

;;; 6.2. Numbers

(define (min x . args)
  (let loop ((pivot x) (rest args))
    (if (null? rest)
	pivot
	(loop (if (< x (car rest)) x (car rest)) (cdr rest)))))

(define (max x . args)
  (let loop ((pivot x) (rest args))
    (if (null? rest)
	pivot
	(loop (if (> x (car rest)) x (car rest)) (cdr rest)))))

(define (floor/ n m)
  (values (floor-quotient n m)
	  (floor-remainder n m)))

(define (truncate/ n m)
  (values (truncate-quotient n m)
	  (truncate-remainder n m)))

(define (exact-integer-sqrt k)
  (let ((n (exact (sqrt k))))
    (values n (- k (square n)))))

;;; 6.3 Booleans

(define (boolean=? . objs)
  (or (every (lambda (x) (eq? x #t)) objs)
      (every (lambda (x) (eq? x #f)) objs)))

;;; 6.5. Symbols

(define (symbol=? . objs)
  (let ((sym (car objs)))
    (if (symbol? sym)
	(every (lambda (x)
		 (and (symbol? x)
		      (eq? x sym)))
	       (cdr objs))
	#f)))

;;; 6.6 Characters

(define-macro (define-char-transitive-predicate name op)
  `(define (,name . cs)
     (apply ,op (map char->integer cs))))

(define-char-transitive-predicate char=? =)
(define-char-transitive-predicate char<? <)
(define-char-transitive-predicate char>? >)
(define-char-transitive-predicate char<=? <=)
(define-char-transitive-predicate char>=? >=)

;;; 6.7 String

(define (string . objs)
  (let ((len (length objs)))
    (let ((v (make-string len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((< i len)
	   v)
	(string-set! v i (car l))))))

(define (string->list string . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (string-length string))))
    (do ((i start (+ i 1))
	 (res '()))
	((< i end)
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
	((< j end))
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
	((< i end)
	 #f)
      (string-set! v i fill))))

;;; 6.8. Vector

(define (vector . objs)
  (let ((len (length objs)))
    (let ((v (make-vector len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((< i len)
	   v)
	(vector-set! v i (car l))))))

(define (vector->list vector . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (vector-length vector))))
    (do ((i start (+ i 1))
	 (res '()))
	((< i end)
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
	((< j end))
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
	((< i end)
	 #f)
      (vector-set! v i fill))))

(define (vector->string . args)
  (list->string (apply vector->list args)))

(define (string->vector . args)
  (list->vector (apply string->list args)))

;;; 6.9 bytevector

(define (bytevector . objs)
  (let ((len (length objs)))
    (let ((v (make-bytevector len)))
      (do ((i 0 (+ i 1))
	   (l objs (cdr l)))
	  ((< i len)
	   v)
	(bytevector-u8-set! v i (car l))))))

(define (bytevector-copy! to at from . opts)
  (let ((start (if (pair? opts) (car opts) 0))
	(end (if (>= (length opts) 2)
		 (cadr opts)
		 (bytevector-length from))))
    (do ((i at (+ i 1))
	 (j start (+ j 1)))
	((< j end))
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

;;; hygienic macros

(define (sc-macro-transformer f)
  (lambda (expr use-env mac-env)
    (make-syntactic-closure mac-env '() (f expr use-env))))

(define (rsc-macro-transformer f)
  (lambda (expr use-env mac-env)
    (make-syntactic-closure use-env '() (f expr mac-env))))

(define (er-macro-transformer f)
  (lambda (expr use-env mac-env)
    (define (rename identifier)
      (make-syntactic-closure mac-env '() identifier))
    (define (compare x y)
      (identifier=? use-env x use-env y))
    (make-syntactic-closure use-env '() (f expr rename compare))))

(define (acons key val alist)
  (cons (cons key val) alist))

(define (walk f obj)
  (if (pair? obj)
      (cons (walk f (car obj)) (walk f (cdr obj)))
      (if (vector? obj)
	  (list->vector (map (lambda (x) (walk f x)) (vector->list obj)))
	  (f obj))))

(define (ir-macro-transformer f)
  (lambda (expr use-env mac-env)
    (let ((wrapped '()))
      (define (inject obj)
        (let ((s (make-syntactic-closure use-env '() obj)))
          (set! wrapped (acons s obj wrapped))
          s))
      (define (extract obj)
        (let ((t (assq obj wrapped)))
          (if t (cdr t) obj)))
      (define (wrap expr)
        (walk inject expr))
      (define (unwrap expr)
        (walk extract expr))
      (define (compare x y)
        (identifier=? use-env x use-env y))
      (make-syntactic-closure mac-env '() (unwrap (f (wrap expr) inject compare))))))

(define-syntax or
  (er-macro-transformer
   (lambda (expr inject compare)
     (let ((exprs (cdr expr)))
       (if (null? exprs)
           #f
           `(let ((it ,(car exprs)))
              (if it
                  it
                  (or ,@(cdr exprs)))))))))

(define-syntax case
  (er-macro-transformer
   (lambda (expr inject compare)
     (let ((key (cadr expr))
           (clauses (cddr expr)))
       `(let ((key ,key))
          ,(let loop ((clauses clauses))
               (if (null? clauses)
                   '#f
                   `(if (or ,@(map (lambda (x) `(eqv? key ,x)) (caar clauses)))
                        ,@(cdar clauses)
                        ,(loop (cdr clauses))))))))))
