(define-library (picrin logic)
  (import (scheme base)
          (picrin control))
  (export call/fresh
          disj
          conj
          is
          run-goal
          run-goal*
          zero
          plus
          unit
          bind
          reify
          reflect)

  (define (assp p alist)
    (if (null? alist)
        #f
        (if (p (caar alist))
            (car alist)
            (assp p (cdr alist)))))

  (define (force* $)
    (if (procedure? $) (force* ($)) $))

  ;; fundation

  (define (var c) (vector c))
  (define (var? x) (vector? x))
  (define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

  (define (subst u s)
    (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
      (if pr (subst (cdr pr) s) u)))

  (define (subst* v s)
    (let ((v (subst v s)))
      (cond
       ((var? v) v)
       ((pair? v) (cons (subst* (car v) s)
                        (subst* (cdr v) s)))
       (else v))))

  (define (ext-s x v s) `((,x . ,v) . ,s))

  (define (unify u v s)
    (let ((u (subst u s)) (v (subst v s)))
      (cond
       ((and (var? u) (var? v) (var=? u v)) s)
       ((var? u) (ext-s u v s))
       ((var? v) (ext-s v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s))))
       (else (and (eqv? u v) s)))))

  ;; klist monad

  (define zero '())
  (define (plus $1 $2)
    (cond
     ((null? $1) $2)
     ((procedure? $1) (lambda () (plus $2 ($1))))
     ((pair? $1) (cons (car $1) (plus (cdr $1) $2)))))

  (define (unit s/c) (list s/c))
  (define (bind $ g)
    (cond
     ((null? $) zero)
     ((procedure? $) (lambda () (bind ($) g)))
     ((pair? $) (plus (g (car $)) (bind (cdr $) g)))))

  (define-syntax reify
    (syntax-rules ()
      ((_ expr)
       (reset (unit expr)))))

  (define (reflect m)
    (shift k (bind m k)))

  ;; goal constructors

  (define (call/fresh f)
    (lambda (s/c)
      (let ((s (car s/c)) (c (cdr s/c)))
        ((f (var c)) `(,s . ,(+ c 1))))))

  (define (disj g1 g2) (lambda (s/c) (plus (g1 s/c) (g2 s/c))))
  (define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

  (define (is u v)
    (lambda (s/c)
      (let ((s (unify u v (car s/c))))
        (if s (unit `(,s . ,(cdr s/c))) zero))))

  ;; goal runner

  (define initial-state '(() . 0))

  (define (run-goal n g)
    (map reify-1st (take n (g initial-state))))

  (define (run-goal* g)
    (map reify-1st (take* (g initial-state))))

  (define (take n $)
    (if (zero? n) '()
        (let (($ (force* $)))
          (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

  (define (take* $)
    (let (($ (force* $)))
      (if (null? $) '() (cons (car $) (take* (cdr $))))))

  (define (reify-1st s/c)
    (let ((v (subst* (var 0) (car s/c))))
      (subst* v (reify-s v '()))))

  (define (reify-s v s)
    (let ((v (subst v s)))
      (cond
       ((var? v)
        (let  ((n (reify-name (length s))))
          (cons `(,v . ,n) s)))
       ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
       (else s))))

  (define (reify-name n)
    (string->symbol
     (string-append "_" "." (number->string n)))))
