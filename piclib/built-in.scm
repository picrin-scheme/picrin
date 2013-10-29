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
