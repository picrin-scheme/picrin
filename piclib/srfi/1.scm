(define-library (srfi 1)
  (import (scheme base))

  ;; # Constructors
  ;; cons list
  ;; xcons cons* make-list list-tabulate
  ;; list-copy circular-list iota
  (define (xcons a b)
    (cons b a))

  (define (cons* x . args)
    (let rec ((acc '()) (x x) (lst args))
      (if (null? lst)
	  (append-reverse acc x)
	  (rec (cons x acc) (car lst) (cdr lst)))))

  (define (list-tabulate n init-proc)
    (let rec ((acc '()) (n (- n 1)))
      (if (zero? n)
	  (cons n acc)
	  (rec (cons n acc) (- n 1)))))

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
      (let rec ((count (- count 1)) (acc '()))
	(if (zero? count)
	    (cons start acc)
	    (rec (- count 1)
		 (cons (+ start (* count step)) acc))))))

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
    (let rec ((lst list) (acc '()))
      (if (null? lst)
	  acc
	  (let ((rst (cdr lst)))
	    (set-cdr! lst acc)
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

  (define (count pred . clists)
    (let rec ((tflst (apply map pred clists)) (n 0))
      (if (null? tflst)
	  n
	  (rec (cdr tflst) (if (car tflst) (+ n 1) n)))))
  
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
  
  ;; means for inter-referential definition
  (define every #f)
  
  (define (fold kons knil clist . clists)
    (if (null? clists)
	(let rec ((acc knil) (clist clist))
	  (if (null? clist)
	      acc
	      (rec (kons (car clist) acc) (cdr clist))))
	(let rec ((acc knil) (clists (cons clist clists)))
	  (if (every pair? clists)
	      (rec (apply kons (append (map car clists) (list acc)))
		   (map cdr clists))
	      acc))))

  (define (fold-right kons knil clist . clists)
    (if (null? clists)
	(let rec ((clist clist) (cont values))
	  (if (null? clist)
	      (cont knil)
	      (rec (cdr clist) (lambda (x) (cont (kons (car clist) x))))))
	(let rec ((clists (cons clist clists)) (cont values))
	  (if (every pair? clists)
	      (rec (map cdr clists)
		   (lambda (x)
		     (cont (apply kons (append (map car clists) (list x))))))
	      (cont knil)))))

  (define (pair-fold kons knil clist . clists)
    (if (null? clists)
	(let rec ((acc knil) (clist clist))
	  (if (null? clist)
	      acc
	      (let ((tail (cdr clist)))
		(rec (kons clist acc) tail))))
	(let rec ((acc knil) (clists (cons clist clists)))
	  (if (every pair? clists)
	      (let ((tail (map cdr clists)))
		(rec (apply kons (append clists (list acc)))
		     tail))
	      acc))))

  (define (pair-fold-right kons knil clist . clists)
    (if (null? clists)
	(let rec ((clist clist) (cont values))
	  (if (null? clist)
	      (cont knil)
	      (let ((tail (map cdr clists)))
		(rec tail (lambda (x) (cont (kons clist x)))))))
	(let rec ((clists (cons clist clists)) (cont values))
	  (if (every pair? clists)
	      (let ((tail (map cdr clists)))
		(rec tail
		     (lambda (x)
		       (cont (apply kons (append clists (list x)))))))
	      (cont knil)))))

  (define (reduce f ridentity list)
    (if (null? list)
	ridentity
	(fold f (car list) (cdr list))))

  (define (reduce-right f ridentity list)
    (fold-right f ridentity list))
  
  (define (unfold p f g seed . tail-gen)
    (let ((tail-gen (if (null? tail-gen)
			(lambda (x) '())
			(car tail-gen))))
      (let rec ((seed seed) (cont values))
	(if (p seed)
	    (cont (tail-gen seed))
	    (rec (g seed) (lambda (x) (cont (cons (f seed) x))))))))

  (define (unfold-right p f g seed . tail)
    (let rec ((seed seed) (lst tail))
      (if (p seed)
	  lst
	  (rec (g seed) (cons (f seed) lst)))))

  (define (append-map f . clists)
    (apply append (apply map f clists)))

  (define (append-map! f . clists)
    (apply append! (apply map f clists)))

  (define pair-for-each #f)
  
  (define (map! f list . lists)
    (if (null? lists)
	(pair-for-each (lambda (x) (set-car! x (f (car x)))) list)
	(let rec ((list list) (lists lists))
	  (if (pair? list)
	      (let ((head (map car lists))
		    (rest (map cdr lists)))
		(set-car! list (apply f (car list) head))
		(rec (cdr list) tail)))))
    list1)

  (define (map-in-order f clist . clists)
    (if (null? clists)
	(let rec ((clist clist) (acc '()))
	  (if (null? clist)
	      (reverse acc)
	      (rec (cdr clist) (cons (f (car clist)) acc))))
	(let rec ((clists (cons clist clists)) (acc '()))
	  (if (every pair? clists)
	      (rec (map cdr clists)
		   (cons* (apply f (map car clists)) acc))
	      (reverse acc)))))

  (define (pair-for-each f clist . clists)
    (if (null? clist)
	(let rec ((clist clist))
	  (if (pair? clist)
	      (begin (f (car clist)) (rec (cdr clist)))))
	(let rec ((clists (cons clist clists)))
	  (if (every pair? clists)
	      (begin (apply f (map car clists)) (rec (map cdr clists)))))))

  (define (filter-map f clist . clists)
    (if (null? clists)
	(let rec ((clist clist) (cont values))
	  (if (null? clist)
	      (cont '())
	      (rec (cdr clist)
		   (let ((it (f (car clist))))
		     (if it
			 (lambda (x) (cont (cons it x)))
			 (lambda (x) (cont x)))))))))
  
  (export map for-each
	  fold unfold pair-fold reduce
	  fold-right unfold-right pair-fold-right reduce-right
	  append-map append-map!
	  map! pair-for-each filter-map map-in-order)

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
