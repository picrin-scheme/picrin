;;; RAY -- Ray-trace a simple scene with spheres, generating a ".pgm" file.
;;; Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (rnrs files)
        (rnrs arithmetic flonums))

(define (make-point x y z)
  (vector x y z))

(define (point-x p) (vector-ref p 0))
(define (point-y p) (vector-ref p 1))
(define (point-z p) (vector-ref p 2))

(define (sq x) (fl* x x))

(define (mag x y z)
  (flsqrt (fl+ (sq x) (sq y) (sq z))))

(define (unit-vector x y z)
  (let ((d (mag x y z)))
    (make-point (fl/ x d) (fl/ y d) (fl/ z d))))

(define (distance p1 p2)
  (mag (fl- (point-x p1) (point-x p2))
       (fl- (point-y p1) (point-y p2))
       (fl- (point-z p1) (point-z p2))))

(define (minroot a b c)
  (if (flzero? a)
      (fl/ (fl- c) b)
      (let ((disc (fl- (sq b) (fl* 4.0 a c))))
        (if (flnegative? disc)
            #f
            (let ((discrt (flsqrt disc))
                  (minus-b (fl- b))
                  (two-a (fl* 2.0 a)))
              (flmin (fl/ (fl+ minus-b discrt) two-a)
                        (fl/ (fl- minus-b discrt) two-a)))))))

(define *world* '())

(define eye (make-point 0.0 0.0 200.0))

(define (tracer pathname res)
  (if (file-exists? pathname)
      (delete-file pathname))
  (call-with-output-file
   pathname
   (lambda (p)
     (let ((extent (* res 100)))
       (display "P2 " p)
       (write extent p)
       (display " " p)
       (write extent p)
       (display " 255" p)
       (newline p)
       (do ((y 0 (+ y 1)))
           ((= y extent))
         (do ((x 0 (+ x 1)))
             ((= x extent))
           (write (color-at
                   (fl+ -50.0
                           (fl/ (inexact x) (inexact res)))
                   (fl+ -50.0
                           (fl/ (inexact y) (inexact res))))
                  p)
           (newline p)))))))

(define (color-at x y)
  (let ((ray (unit-vector (fl- x (point-x eye))
                          (fl- y (point-y eye))
                          (fl- (point-z eye)))))
    (exact (flround (fl* (sendray eye ray) 255.0)))))



(define (sendray pt ray)
  (let* ((x (first-hit pt ray))
         (s (vector-ref x 0))
         (int (vector-ref x 1)))
    (if s
        (fl* (lambert s int ray)
                (surface-color s))
        0.0)))

(define (first-hit pt ray)
  (let loop ((lst *world*) (surface #f) (hit #f) (dist 1e308))
    (if (null? lst)
        (vector surface hit)
        (let ((s (car lst)))
          (let ((h (intersect s pt ray)))
            (if h
                (let ((d (distance h pt)))
                  (if (fl<? d dist)
                      (loop (cdr lst) s h d)
                      (loop (cdr lst) surface hit dist)))
                (loop (cdr lst) surface hit dist)))))))

(define (lambert s int ray)
  (let ((n (normal s int)))
    (flmax 0.0
              (fl+ (fl* (point-x ray) (point-x n))
                      (fl* (point-y ray) (point-y n))
                      (fl* (point-z ray) (point-z n))))))

(define (make-sphere color radius center)
  (vector color radius center))

(define (sphere-color s) (vector-ref s 0))
(define (sphere-radius s) (vector-ref s 1))
(define (sphere-center s) (vector-ref s 2))

(define (defsphere x y z r c)
  (let ((s (make-sphere c r (make-point x y z))))
    (set! *world* (cons s *world*))
    s))

(define (surface-color s)
  (sphere-color s))

(define (intersect s pt ray)
  (sphere-intersect s pt ray))

(define (sphere-intersect s pt ray)
  (let* ((xr (point-x ray))
         (yr (point-y ray))
         (zr (point-z ray))
         (c (sphere-center s))
         (n (minroot
             (fl+ (sq xr) (sq yr) (sq zr))
             (fl* 2.0
                     (fl+ (fl* (fl- (point-x pt) (point-x c)) xr)
                             (fl* (fl- (point-y pt) (point-y c)) yr)
                             (fl* (fl- (point-z pt) (point-z c)) zr)))
             (fl+ (sq (fl- (point-x pt) (point-x c)))
                     (sq (fl- (point-y pt) (point-y c)))
                     (sq (fl- (point-z pt) (point-z c)))
                     (fl- (sq (sphere-radius s)))))))
    (if n
        (make-point (fl+ (point-x pt) (fl* n xr))
                    (fl+ (point-y pt) (fl* n yr))
                    (fl+ (point-z pt) (fl* n zr)))
        #f)))

(define (normal s pt)
  (sphere-normal s pt))

(define (sphere-normal s pt)
  (let ((c (sphere-center s)))
    (unit-vector (fl- (point-x c) (point-x pt))
                 (fl- (point-y c) (point-y pt))
                 (fl- (point-z c) (point-z pt)))))

(define (ray-test res output-file)
  (set! *world* '())
  (defsphere 0.0 -300.0 -1200.0 200.0 0.8)
  (defsphere -80.0 -150.0 -1200.0 200.0 0.7)
  (defsphere 70.0 -100.0 -1200.0 200.0 0.9)
  (do ((x -2 (+ x 1)))
      ((> x 2))
    (do ((z 2 (+ z 1)))
        ((> z 7))
      (defsphere
        (fl* (inexact x) 200.0)
        300.0
        (fl* (inexact z) -400.0)
        40.0
        0.75)))
  (tracer output-file res))

(define (run input output)
  (ray-test input output)
  'ok)

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "ray"))
    (run-r6rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (run (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))
