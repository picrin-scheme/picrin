(import (scheme base)
        (scheme lazy)
        (scheme write)
        (picrin logic)
        (picrin test))

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (run-goal n (fresh (x ...) g0 g ...)))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (run-goal* (fresh (x ...) g0 g ...)))))

(define (appendo l s out)
  (conde
    ((is '() l) (is s out))
    ((fresh (a d res)
       (is `(,a . ,d) l)
       (is `(,a . ,res) out)
       (appendo d s res)))))

(test '((() (1 2 3 4 5))
        ((1) (2 3 4 5))
        ((1 2) (3 4 5))
        ((1 2 3) (4 5))
        ((1 2 3 4) (5))
        ((1 2 3 4 5) ()))
      (run* (q) (fresh (x y) (is `(,x ,y) q) (appendo x y '(1 2 3 4 5)))))

(test '((() (1 2 3 4 5))
        ((1) (2 3 4 5))
        ((1 2) (3 4 5))
        ((1 2 3) (4 5))
        ((1 2 3 4) (5))
        ((1 2 3 4 5) ()))
      (run* (q x y) (is `(,x ,y) q) (appendo x y '(1 2 3 4 5))))

(test '((1 2 8 3 4 5)
        (1 2 8 3 4 5 8)
        (1 2 8 3 4 8 5)
        (1 2 8 3 8 4 5)
        (1 2 8 8 3 4 5)
        (1 2 8 8 3 4 5)
        (1 8 2 8 3 4 5)
        (8 1 2 8 3 4 5))
      (letrec
          ((rember*o (lambda (tr o)
                       (conde
                        ((is '() tr) (is '() o))
                        ((fresh (a d)
                                (is `(,a . ,d) tr)
                                (conde
                                 ((fresh (aa da)
                                         (is `(,aa . ,da) a)
                                         (fresh (a^ d^)
                                                (rember*o a a^)
                                                (rember*o d d^)
                                                (is `(,a^ . ,d^) o))))
                                 ((is a 8) (rember*o d o))
                                 ((fresh (d^)
                                         (rember*o d d^)
                                         (is `(,a . ,d^) o))))))))))
        (run 8 (q) (rember*o q '(1 2 8 3 4 5)))))

(test '((1 (2 8 3 4) 5)
        (1 (2 8 3 4) 5 8)
        (1 (2 8 3 4) 5 8 8)
        (1 (2 8 3 4) 8 5)
        (1 8 (2 8 3 4) 5)
        (8 1 (2 8 3 4) 5)
        (1 (2 8 3 4) 5 8 8 8)
        (1 (2 8 3 4) 5 8 8 8 8)
        (1 (2 8 3 4) 5 8 8 8 8 8))
      (letrec
          ((rember*o (lambda (tr o)
                       (conde
                        ((is '() tr) (is '() o))
                        ((fresh (a d)
                                (is `(,a . ,d) tr)
                                (conde
                                 ((fresh (aa da)
                                         (is `(,aa . ,da) a)
                                         (fresh (a^ d^)
                                                (is `(,a^ . ,d^) o)
                                                (rember*o d d^)
                                                (rember*o a a^))))
                                 ((is a 8) (rember*o d o))
                                 ((fresh (d^)
                                         (is `(,a . ,d^) o)
                                         (rember*o d d^))))))))))
        (run 9 (q) (rember*o q '(1 (2 8 3 4) 5)))))
