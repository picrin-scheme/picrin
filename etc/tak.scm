(import (scheme base)
        (scheme time)
        (scheme write))

(define (time f)
  (let ((start (current-jiffy)))
    (f)
    (inexact
     (/ (- (current-jiffy) start)
        (jiffies-per-second)))))

(define (tak x y z)
  (if (> x y)
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))
      y))

(define (f)
  (tak 12 6 0))

(write-simple (time f))
(newline)

; 70fb34 -> 10.374959
; fb6679 ->  4.275342
