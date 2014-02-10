(define-library (srfi 1)
  (import (scheme base))

  ;; # Constructors
  ;; cons list
  ;; xcons cons* make-list list-tabulate
  ;; list-copy circular-list iota
  (define (xcons a b)
    (cons b a))

  (define (cons* x . args)
    (let rec ((acm '()) (x x) (lst args))
      (if (null? lst)
	  (append-reverse acm x)
	  (rec (cons x acm) (car lst) (cdr lst)))))

  (define (list-tabulate n init-proc)
    (let rec ((acm '()) (n (- n 1)))
      (if (zero? n)
	  (cons n acm)
	  (rec (cons n acm) (- n 1)))))

  (define (circular-list elt . args)
    (let ((lst (cons elt args)))
      (let rec ((l lst))
	(if (null? (cdr l))
	    (set-cdr! l lst)
	    (rec (cdr l))))
      lst))

  (define (iota count . lst)
    (let ((start (if (pair? lst) (car lst) 0))
	  (step (if (and (pair? lst) (pair? (cdr lst)))
		    (cadr lst) 1)))
      (let rec ((count (- count 1)) (acm '()))
	(if (zero? count)
	    (cons start acm)
	    (rec (- count 1)
		 (cons (+ start (* count step)) acm))))))

  (export cons list xcons make-list list-tabulate list-copy circular-list iota)

  ;; # Predicates
  ;; pair? null?
  ;; proper-list? circular-list? dotted-list?
  ;; not-pair? null-list?
  ;; list=
  (define (not-pair? x)
    (not (pair? x)))

  (define (circular-list? x)
    (and (pair? x)
	 (let rec ((lst (cdr x)))
	   (cond ((not-pair?) #f)
		 ((null? lst) #f)
		 ((eq? x lst) #t)
		 (else (rec (cdr lst)))))))

  (define (proper-list? x)
    (if (not (circular-list? x))
	(list? x)))

  (define (dotted-list? x)
    (and (pair? x)
	 (not (proper-list? x))
	 (not (circular-list? x))))

  (define (null-list? x)
    (cond ((pair? x) #f)
	  ((null? x) #t)
	  (else (error "null-list?: argument out of domain" x))))

  (define (list= elt= . lists)
    (or (null? lists)
	(let rec1 ((list1 (car lists)) (others (cdr lists)))
	  (or (null? others)
	      (let ((list2 (car others))
		    (others (cdr others)))
		(if (eq? list1 list2)
		    (rec1 list2 others)
		    (let rec2 ((l1 list1) (l2 list2))
		      (if (null-list? l1)
			  (and (null-list? l2)
			       (rec1 list2 others))
			  (and (not (null-list? l2))
			       (elt= (car l1) (car l2))
			       (rec2 (cdr l1) (cdr l2)))))))))))

  (export pair? null? not-pair? proper-list? circular-list? null-list? list=)

  ;; # Selectors
  ;; car cdr ... cddadr cddddr list-ref
  ;; first second third fourth fifth sixth seventh eighth ninth tenth
  ;; car+cdr
  ;; take drop
  ;; take-right drop-right
  ;; take! drop-right!
  ;; split-at split-at!
  ;; last last-pair
  (define (car+cdr pair)
    (values (car pair) (cdr pair)))

  (define (take x i)
    (if (zero? i)
        '()
        (cons (car x)
              (take (cdr x) (- i 1)))))

  (define (drop x i)
    (if (zero? i)
        x
        (drop (cdr x) (- i 1))))

  (define (take-right flist i)
    (let ((len (length flist)))
      (drop flist (- len i))))

  (define (drop-right flist i)
    (let ((len (length flist)))
      (take flist (- len i))))

  (define (take! x i)
    (let rec ((lis x) (n (- i 1)))
      (if (zero? n)
	  (begin (set-cdr! lis '()) x)
	  (rec (cdr lis) (- n 1)))))

  ;;
  (define (drop-right! flist i)
    (let ((lead (drop flist i)))
      (if (not-pair? lead)
	  '()
	  (let rec ((lis1 flist) (lead (cdr lead)))
	    (if (pair? lis2)
		(rec (cdr lis1) (cdr lis2))
		(begin (set-cdr! lis1 '()) flist))))))
    
  (define (split-at x i)
    (values (take x i) (drop x i)))

  (define (split-at! x i)
    (values (take! x i) (drop x i)))

  (define (last pair)
    (car (take-right pair 1)))

  (define (last-pair pair)
    (take-right pair 1))
  
  (define first car)
  (define second cadr)
  (define third caddr)
  (define fourth cadddr)
  (define (fifth pair)
    (list-ref pair 5))
  (define (sixth pair)
    (list-ref pair 6))
  (define (seventh pair)
    (list-ref pair 7))
  (define (eighth pair)
    (list-ref pair 8))
  (define (ninth pair)
    (list-ref pair 9))
  (define (tenth pair)
    (list-ref pair 10))
  
  (export car cdr car+cdr list-ref
	  caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
	  cdadar cdaddr cddaar cddadr cdddar cddddr
	  first second third fourth fifth sixth seventh eighth ninth tenth
          take drop take-right drop-right take! drop-right!
	  split-at split-at! last last-pair)

  ;; # Miscellaneous
  ;; length length+
  ;; append concatenate reverse
  ;; append! concatenate! reverse!
  ;; append-reverse append-reverse!
  ;; zip unzip1 unzip2 unzip3 unzip4 unzip5
  ;; count
  (define (length+ lst)
    (if (not (circular-list? lst))
	(length lst)))
  
  (define (concatenate lists)
    (apply append lists))

  (define (append! . lists)
    (if (null? lists)
	'()
	(let rec ((lst lists))
	  (if (not-pair? (cdr lst))
	      (car lst)
	      (begin (set-cdr! (last-pair (car lst)) (cdr lst))
		     (rec (cdr lst)))))))

  (define (concatenate! lists)
    (apply append! lists))

  (define (reverse! list)
    (let rec ((lst list) (acm '()))
      (if (null? lst)
	  acm
	  (let ((rst (cdr lst)))
	    (set-cdr! lst acm)
	    (rec rst lst)))))
  
  (define (append-reverse rev-head tail)
    (if (null? rev-head)
	tail
	(append-reverse (cdr rev-head) (cons (car rev-head) tail))))

  (define (append-reverse! rev-head tail)
    (let ((rst (cdr rev-head)))
      (if (null? rev-head)
	  tail
	  (begin (set-cdr! rev-head tail)
		 (append-reverse! rst rev-head)))))

  (define (zip . lists)
    (apply map list lists))

  (define (unzip1 list)
    (map first list))

  (define (unzip2 list)
    (values (map first list)
	    (map second list)))

  (define (unzip3 list)
    (values (map first list)
	    (map second list)
	    (map third list)))

  (define (unzip4 list)
    (values (map first list)
	    (map second list)
	    (map third list)
	    (map fourth list)))

  (define (unzip3 list)
    (values (map first list)
	    (map second list)
	    (map third list)
	    (map fourth list)
	    (map fifth list)))
  
  (export length length+
	  append append! concatenate concatenate!
	  reverse reverse! append-reverse append-reverse!
	  zip unzip1 unzip2 unzip3 unzip4 unzip5)

  ;; # Fold, unfold & map
  ;; map for-each
  ;; fold unfold pair-fold reduce
  ;; fold-right unfold-right pair-fold right reduce-right
  ;; append-map append-map!
  ;; map! pair-for-each filter-map map-in-order
  
  (export map for-each)

  ;; # Filtering & partitioning
  ;; filter partition remove
  ;; filter! partition! remove!
  (define (filter pred list)
    (if (null? list)
        '()
        (if (pred (car list))
            (cons (car list)
                  (filter pred (cdr list)))
            (filter pred (cdr list)))))

  (define (partition pred list)
    (values (filter pred list)
            (filter (lambda (x) (not (pred x))) list)))

  (define (remove pred list)
    (filter (lambda (x) (not (pred x))) list))

  (export filter partition remove)

  ;; # Searching
  ;; member memq memv
  ;; find find-tail
  ;; any every
  ;; list-index
  ;; take-while drop-while take-while!
  ;; span break span! break!
  (define (find-tail pred list)
    (if (null? list)
        #f
        (if (pred (car list))
            list
            (find-tail pred (cdr list)))))

  (define (find pred list)
    (let ((tail (find-tail pred list)))
      (if tail
          (car tail)
          #f)))

  (export member memq memv find-tail find)

  ;; # Deleting
  ;; delete delete-duplicates
  ;; delete! delete-duplicates!

  ;; # Association lists
  ;; assoc assq assv
  ;; alist-cons alist-copy
  ;; alist-delete alist-delete!
  (export assoc assq assv)

  ;; # Set operations on lists
  ;; lset<= lset= lset-adjoin
  ;; lset-union lset-union!
  ;; lset-intersection lset-intersection!
  ;; lset-difference lset-difference!
  ;; lset-xor lset-xor!
  ;; lset-diff+intersenction lset-diff+intersection!

  ;; # Primitive side-effects
  ;; set-car! set-cdr!
  (export set-car! set-cdr!))

