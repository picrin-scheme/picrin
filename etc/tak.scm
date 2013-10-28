(define start #f)

(define end #f)

(define (time f)
  (set! start (current-jiffy))
  (f)
  (set! end (current-jiffy))
  (/ (- end start) (jiffies-per-second)))

(define (tak x y z)
  (if (> x y)
      (tak (tak (- x 1) y z) 
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))
      y))

(define (f)
  (tak 12 6 0))

(write (time f))
