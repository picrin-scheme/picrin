(import (scheme base)
        (scheme write))

(begin

 (define foo (lambda (a)
		(lambda ()
		  a)))
 (define bar (foo 1))

 ; must be 1
 (write (bar))
 (newline)

 (define baz (foo 2))

 ; must be 2
 (write (baz))
 (newline)

 ; must be 1
 (write (bar))
 (newline))
