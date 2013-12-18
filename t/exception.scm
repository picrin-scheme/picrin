(import (scheme base)
        (scheme write))

(define (print obj)
  (write obj)
  (newline)
  obj)

(print
 (call/cc
  (lambda (k)
    (with-exception-handler
     (lambda (x)
       (write "condition: ")
       (write x)
       (newline)
       (k 'exception))
     (lambda ()
       (+ 1 (raise 'an-error)))))))

(print
 (with-exception-handler
  (lambda (con)
    (cond
     ((string? con)
      (print con))
     (else
      (print "a warning has been issued")))
    42)
  (lambda ()
    (+ (raise-continuable "should be a number")
       23))))
