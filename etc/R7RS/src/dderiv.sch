;;; DDERIV -- Table-driven symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(import (rnrs base)
        (rnrs io simple)
        (rnrs hashtables)
        (rnrs mutable-pairs))
  
(define (lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define properties (make-hashtable symbol-hash eq?))

(define (get key1 key2)
  (let ((x (hashtable-ref properties key1 #f)))
    (if x
      (let ((y (lookup key2 x)))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (hashtable-ref properties key1 #f)))
    (if x
      (let ((y (lookup key2 x)))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (hashtable-set! properties key1 (list (cons key2 val))))))

(define (my+dderiv a)
  (cons '+
        (map dderiv (cdr a))))

(define (my-dderiv a)
  (cons '-
        (map dderiv (cdr a))))

(define (*dderiv a)
  (list '*
         a
         (cons '+
               (map (lambda (a) (list '/ (dderiv a) a)) (cdr a)))))

(define (/dderiv a)
  (list '-
        (list '/
              (dderiv (cadr a))
              (caddr a))
        (list '/
              (cadr a)
              (list '*
                    (caddr a)
                    (caddr a)
                    (dderiv (caddr a))))))

(put '+ 'dderiv my+dderiv)
(put '- 'dderiv my-dderiv)
(put '* 'dderiv *dderiv)
(put '/ 'dderiv /dderiv)

(define (dderiv a)
  (if (not (pair? a))
    (if (eq? a 'x) 1 0)
    (let ((f (get (car a) 'dderiv)))
      (if f
        (f a)
        (error #f "No derivation method available")))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s (number->string count))
         (name "dderiv"))
    (run-r6rs-benchmark
     (string-append name ":" s)
     count
     (lambda () (dderiv (hide count input1)))
     (lambda (result) (equal? result output)))))


