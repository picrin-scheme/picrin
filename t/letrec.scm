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
