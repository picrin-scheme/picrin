;;; PI -- Compute PI using bignums.

; See http://mathworld.wolfram.com/Pi.html for the various algorithms.

(import (rnrs base)
        (rnrs io simple))

; Utilities.

(define (width x)
  (let loop ((i 0) (n 1))
    (if (< x n) i (loop (+ i 1) (* n 2)))))

(define (root x y)
  (let loop ((g (expt
                 2
                 (div (+ (width x) (- y 1)) y))))
    (let ((a (expt g (- y 1))))
      (let ((b (* a y)))
        (let ((c (* a (- y 1))))
          (let ((d (div (+ x (* g c)) b)))
            (if (< d g) (loop d) g)))))))

(define (square-root x)
  (root x 2))

(define (quartic-root x)
  (root x 4))

(define (square x)
  (* x x))

; Compute pi using the 'brent-salamin' method.

(define (pi-brent-salamin nb-digits)
  (let ((one (expt 10 nb-digits)))
    (let loop ((a one)
               (b (square-root (div (square one) 2)))
               (t (div one 4))
               (x 1))
      (if (= a b)
          (div (square (+ a b)) (* 4 t))
          (let ((new-a (div (+ a b) 2)))
            (loop new-a
                  (square-root (* a b))
                  (- t
                            (div
                             (* x (square (- new-a a)))
                             one))
                  (* 2 x)))))))

; Compute pi using the quadratically converging 'borwein' method.

(define (pi-borwein2 nb-digits)
  (let* ((one (expt 10 nb-digits))
         (one^2 (square one))
         (one^4 (square one^2))
         (sqrt2 (square-root (* one^2 2)))
         (qurt2 (quartic-root (* one^4 2))))
    (let loop ((x (div
                   (* one (+ sqrt2 one))
                   (* 2 qurt2)))
               (y qurt2)
               (p (+ (* 2 one) sqrt2)))
      (let ((new-p (div (* p (+ x one))
                                    (+ y one))))
        (if (= x one)
            new-p
            (let ((sqrt-x (square-root (* one x))))
              (loop (div
                     (* one (+ x one))
                     (* 2 sqrt-x))
                    (div
                     (* one (+ (* x y) one^2))
                     (* (+ y one) sqrt-x))
                    new-p)))))))

; Compute pi using the quartically converging 'borwein' method.

(define (pi-borwein4 nb-digits)
  (let* ((one (expt 10 nb-digits))
         (one^2 (square one))
         (one^4 (square one^2))
         (sqrt2 (square-root (* one^2 2))))
    (let loop ((y (- sqrt2 one))
               (a (- (* 6 one) (* 4 sqrt2)))
               (x 8))
      (if (= y 0)
          (div one^2 a)
          (let* ((t1 (quartic-root (- one^4 (square (square y)))))
                 (t2 (div
                      (* one (- one t1))
                      (+ one t1)))
                 (t3 (div
                      (square (div (square (+ one t2)) one))
                      one))
                 (t4 (+ one
                               (+ t2
                                         (div (square t2) one)))))
            (loop t2
                  (div
                   (- (* t3 a) (* x (* t2 t4)))
                   one)
                  (* 4 x)))))))

; Try it.

(define (pies n m s)
  (if (< m n)
      '()
      (let ((bs (pi-brent-salamin n))
            (b2 (pi-borwein2 n))
            (b4 (pi-borwein4 n)))
        (cons (list b2 (- bs b2) (- b4 b2))
              (pies (+ n s) m s)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "pi"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (pies (hide count input1)
             (hide count input2)
             (hide count input3)))
     (lambda (result) (equal? result output)))))
