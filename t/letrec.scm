(import (scheme base)
        (scheme write))

(define (print obj)
  (write obj)
  (newline)
  obj)

(letrec ((my-odd? (lambda (n)
		    (if (= n 0)
			#t
			(not (my-even? (- n 1))))))
	 (my-even? (lambda (n)
		     (if (= n 0)
			 #t
			 (not (my-odd? (- n 1)))))))
  (print '(my-odd? 42))
  (print (my-odd? 42))
  (print '(my-even? 57))
  (print (my-even? 57)))

(print 70)
(print
 (let ((x 2)
       (y 3))
   (let* ((x 7)
	  (z (+ x y)))
     (* z x))))

(print 5)
(print
 (letrec ((p
	   (lambda (x)
	     (+ 1 (q (- x 1)))))
	  (q
	   (lambda (y)
	     (if (zero? y)
		 0
		 (+ 1 (p (- y 1))))))
	  (x (p 5))
	  (y x))
   y))

;; (let ()
;;   (define my-odd? (lambda (n)
;; 		    (if (= n 0)
;; 			#t
;; 			(not (my-even? (- n 1))))))
;;   (define my-even? (lambda (n)
;; 		     (if (= n 0)
;; 			 #t
;; 			 (not (my-odd? (- n 1))))))
;;   (print (my-odd? 42))
;;   (print (my-even? 57)))
