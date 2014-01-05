(import (scheme base)
        (scheme write))

(define (sum k acc)
  (if (zero? k)
      acc
      (sum (- k 1) (+ k acc))))

(write (sum 1000 0))
(newline)
