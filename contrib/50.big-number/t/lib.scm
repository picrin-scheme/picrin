(define-library (picrin big-number)
  (import (scheme base))
  
  (define (bigint-pow a b)
    (if (bigint-less? b 0)
	(make-bigint 1)
	(bigint-pow-positive a b (make-bigint 1))))
  (define (bigint-pow-positive a b sum)
    (if (bigint-less? b 1)
	sum
	(let ((aa (bigint-mul a a)))
	  (if (bigint-equal? (bigint-rem b 2) 0)
	      (bigint-pow-positive aa (bigint-div b 2) sum)
	      (bigint-pow-positive aa (bigint-div b 2) (bigint-mul sum a))))))
	      
  (export bigint-pow))
