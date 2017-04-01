(import (scheme base)
        (scheme read)
        (scheme file)
        (scheme lazy)
        (scheme write)
        (srfi 1)
        (picrin base)
        (picrin test))

(test-begin)

(define trace '())

(define task-queue (make-parameter '() (lambda (x) (set! trace (cons x trace)) x)))

(define expand
  (lambda ()
    (task-queue)))

(define result (expand))
(test '() result)
(test '(()) trace)

(test-end)
