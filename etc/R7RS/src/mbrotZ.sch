;;; MBROT -- Generation of Mandelbrot set fractal
;;; using Scheme's complex numbers.
  
(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

(define (count z0 step z)

  (let* ((max-count 64)
         (radius    4.0)
         (radius^2  (fl* radius radius)))

    (let ((z0 (+ z0 (* z step))))
      
      (let loop ((z z0)
                 (c 0))
        (if (= c max-count)
            c
            (let* ((zr (real-part z))
                   (zi (imag-part z))
                   (zr^2 (fl* zr zr))
                   (zi^2 (fl* zi zi)))
              (if (fl>? (fl+ zr^2 zi^2) radius^2)
                  c
                  (loop (+ (* z z) z0) (+ c 1)))))))))

(define (mbrot matrix z0 step n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
        (let loop2 ((x (- n 1)))
          (if (>= x 0)
              (begin
               (vector-set! (vector-ref matrix x)
                            y
                            (count z0
                                   step
                                   (make-rectangular (inexact x)
                                                     (inexact  y))))
               (loop2 (- x 1)))
            (loop1 (- y 1)))))))

(define (test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (if (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0-0.5i 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "mbrotZ"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (test (hide count input1)))
     (lambda (result) (= result output)))))
