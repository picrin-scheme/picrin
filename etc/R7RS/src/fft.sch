;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"
  
(import (rnrs base)
        (rnrs io simple)
        (rnrs arithmetic flonums))

;(define flsin sin)

(define (four1 data)
  (let ((n (vector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ; bit-reversal section

    (let loop1 ((i 0) (j 0))
      (if (< i n)
        (begin
          (if (< i j)
            (begin
              (let ((temp (vector-ref data i)))
                (vector-set! data i (vector-ref data j))
                (vector-set! data j temp))
              (let ((temp (vector-ref data (+ i 1))))
                (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
                (vector-set! data (+ j 1) temp))))
          (let loop2 ((m (div n 2)) (j j))
            (if (and (>= m 2) (>= j m))
              (loop2 (div m 2) (- j m))
              (loop1 (+ i 2) (+ j m)))))))

    ; Danielson-Lanczos section

    (let loop3 ((mmax 2))
      (if (< mmax n)
        (let* ((theta
                (fl/ pi*2 (inexact mmax)))
               (wpr
                (let ((x (flsin (fl* 0.5 theta))))
                  (fl* -2.0 (fl* x x))))
               (wpi
                (flsin theta)))
          (let loop4 ((wr 1.0) (wi 0.0) (m 0))
            (if (< m mmax)
              (begin
                (let loop5 ((i m))
                  (if (< i n)
                    (let* ((j
                            (+ i mmax))
                           (tempr
                            (fl-
                              (fl* wr (vector-ref data j))
                              (fl* wi (vector-ref data (+ j 1)))))
                           (tempi
                            (fl+
                              (fl* wr (vector-ref data (+ j 1)))
                              (fl* wi (vector-ref data j)))))
                      (vector-set! data j
                        (fl- (vector-ref data i) tempr))
                      (vector-set! data (+ j 1)
                        (fl- (vector-ref data (+ i 1)) tempi))
                      (vector-set! data i
                        (fl+ (vector-ref data i) tempr))
                      (vector-set! data (+ i 1)
                        (fl+ (vector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)));***))
                (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
                       (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
                       (+ m 2)))))
));******
          (loop3 (* mmax 2)))))))

(define data
  (make-vector 1024 0.0))
 
(define (run data)
  (four1 data)
  (vector-ref data 0))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "fft"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (run (hide count (make-vector input1 input2))))
     (lambda (result) (equal? result output)))))
