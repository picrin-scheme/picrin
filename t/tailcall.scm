(define (sum k acc)
  (if (zero? k)
      acc
      (sum (- k 1) (+ k acc))))

(display (sum 1000 0))
