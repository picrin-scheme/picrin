(import (scheme base))

;;; always returns zero
(define (zero n)
  (if (zero? n)
      0
      (zero (- n 1))))

;;; using apply
(define (zero-2 n)
  (if (zero? n)
      0
      (apply zero-2 (list (- n 1)))))

(zero-2 100000)
