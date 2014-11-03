(import (scheme base)
        (picrin test))

(test-begin)

(define-syntax fard
  (syntax-rules ()
    ((fard a b) (- a b))))

(test -1 (fard 1 2))

(define (fard a b)
  (+ a b))

(test 3 (fard 1 2))

(test 3 (apply fard (list 1 2)))

(test-end)
