(define-library (picrin parser)
  (import (scheme base)
          (picrin control)
          (picrin procedure))
  (export parse
          ;; monadic
          reify
          reflect
          bind
          unit
          zero
          plus
          fapply
          ;; look ahead
          with
          without
          ;; eta
          lazy
          ;; aux
          choice
          optional
          many
          between)

  ;; type Parser i r = i -> Maybe (r, i)

  (define (parse rule input)
    (rule input))

  ;; monadic operators

  (define-syntax reify
    (syntax-rules ()
      ((_ expr)
       (reset (unit expr)))))

  (define (reflect x)
    (shift k (bind x k)))

  (define (bind m f)
    (lambda (i)
      (let ((x (m i)))
        (and x ((f (car x)) (cdr x))))))

  (define (unit x)
    (lambda (i)
      `(,x . ,i)))

  (define zero
    (lambda (i) #f))

  (define (plus a b)
    (lambda (i)
      (or (a i) (b i))))

  (define (fapply f . args)
    (reify
     (let loop ((args args) (ps '()))
       (if (null? args)
           (apply f (reverse ps))
           (loop (cdr args) (cons (reflect (car args)) ps))))))

  ;; look ahead

  (define (with a)
    (lambda (i)
      (and (a i) `(#f . ,i))))

  (define (without a)
    (lambda (i)
      (and (not (a i)) `(#f . ,i))))

  ;; eta conversion

  (define-syntax lazy
    (syntax-rules ()
      ((_ expr)
       (lambda (i) (expr i)))))

  ;; aux

  (define (choice . xs)
    (if (null? xs)
        zero
        (plus (car xs) (apply choice (cdr xs)))))

  (define (optional a)
    (choice a (unit #f)))

  (define (many a)
    (lazy
     (choice
      (reify
       (let* ((a (reflect a))
              (b (reflect (many a))))
         (cons a b)))
      null)))

  (define (between l x r)
    (fapply (>> list cadr) l x r)))
