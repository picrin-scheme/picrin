(import (scheme base)
        (scheme write)
        (scheme case-lambda)
        (scheme cxr)
        (scheme lazy)
        (srfi 1)
        (srfi 17)
        (expand))

; util

(define-syntax push!
  (syntax-rules ()
    ((push! obj list)
     (set! list (cons obj list)))))

(define (print x)
  (write x)
  (newline)
  x)

(define-syntax p
  (syntax-rules ()
    ((_ expr)
     (begin
       (print 'expr)
       (print expr)
       (newline)))))


; normalize

(define normalize
  (let ((defs (make-parameter '())))

    ;; 1. eliminates internal definitions by replacing them with equivalent let & set!
    ;; 2. transform a var into (ref var)

    ;; TODO: warn redefinition

    (define (normalize e)
      (if (symbol? e)
          `(ref ,e)
          (case (car e)
            ((quote) e)
            ((define)
             (let ((var (second e)) (val (third e)))
               (push! var (defs))
               `(set! ,var ,(normalize val))))
            ((lambda)
             (let ((args (second e)) (body (third e)))
               (parameterize ((defs '()))
                 (let ((body (normalize body)))
                   `(lambda ,args
                      ,(let loop ((xs (defs)))
                         (if (null? xs)
                             body
                             `(let ,(car x) '#f ,(loop (cdr xs))))))))))
            ((set! if begin)
             `(,(car e) . ,(map normalize (cdr e))))
            (else
             (map normalize e)))))

    normalize))


; transform

(define transform
  (let ()

    (define uniq
      (let ((n 0))
        (lambda ()
          (set! n (+ n 1))
          (string->symbol
           (string-append "$" (number->string n))))))

    (define (transform e k)
      (case (car e)
        ((ref)    (transform-ref e k (uniq)))
        ((quote)  (transform-quote e k (uniq)))
        ((set!)   (transform-set! (second e) (third e) k (uniq) (uniq)))
        ((begin)  (transform-begin (second e) (third e) k))
        ((if)     (transform-if (second e) (third e) (fourth e) k (uniq) (uniq) (uniq) (uniq)))
        ((lambda) (transform-lambda (second e) (third e) k (uniq) (uniq)))
        (else     (transform-application e k (uniq) (uniq)))))

    (define (transform-ref e k x)
      `(let ,x ,e ,(k x)))

    (define (transform-quote e k x)
      `(let ,x ,e ,(k x)))

    (define (transform-set! var val k x y)
      (transform val
       (lambda (t)
         `(let ,x (set! ,var ,t)
               (let ,y (quote #f) ,(k y))))))

    (define (transform-begin e1 e2 k)
      (transform e1
       (lambda (t)
         (transform e2 k))))

    (define (transform-application es k x y)
      (let rec ((es es) (acc '()))
        (if (null? es)
            (let ((xs (reverse acc)))
              `(let ,x (lambda (,y) ,(k y))
                 (,(car xs) ,x . ,(cdr xs))))
            (transform (car es)
             (lambda (t)
               (rec (cdr es) `(,t ,@acc)))))))

    (define (transform-lambda args body k f x)
      `(let ,f (lambda (,x ,@args) ,(transform body (lambda (e) `(,x ,e)))) ,(k f)))

    (define (transform-if e0 e1 e2 k x k0 k1 k2)
      (transform e0
       (lambda (t)
         `(let ,k0 (lambda (,x) ,(k x))
               (let ,k1 (lambda () ,(transform e1 (lambda (e) `(,k0 ,e))))
                    (let ,k2 (lambda () ,(transform e2 (lambda (e) `(,k0 ,e))))
                         (if ,t ,k1 ,k2)))))))

    transform))


; codegen

(define codegen
  (let ()

    (define id 0)

    (define table '())

    (define queue (make-parameter '()))

    (define asm '())

    (define (emit-inst x)
      (push! x asm))

    (define (emit-lambda args body)
      (set! id (+ id 1))
      (queue (append (queue) `((,id ,args ,body))))
      id)

    (define (emit-expr* e)
      (if (eq? (car e) 'let)
          (let ((i (second e))
                (v (third e))
                (f (fourth e)))
            (case (car v)
              ((ref) (emit-inst `(ref ,i ,(second v))))
              ((quote) (emit-inst `(const ,i ,(second v))))
              ((set!) (emit-inst `(set! ,(second v) ,(third v))))
              ((lambda) (emit-lambda (second v) (third v)) (emit-inst `(lambda ,i ,id))))
            (emit-expr* f))
          (case (car e)
            ((halt) (emit-inst `(halt ,(second e))))
            ((if) (emit-inst `(if ,(second e) ,(third e) ,(fourth e))))
            (else (emit-inst `(call ,e))))))

    (define (emit-expr e)
      (set! asm '())
      (emit-expr* e)
      (reverse asm))

    (define (emit e)
      (set! id 0)
      (set! table '())
      (queue `((0 () ,e)))
      (let loop ()
        (unless (null? (queue))
          (let ((id   (first (car (queue))))
                (args (second (car (queue))))
                (e    (third (car (queue)))))
            (queue (cdr (queue)))
            (set! table `((,id ,args ,(emit-expr e)) . ,table))
            (loop))))
      (reverse table))

    emit))


; target

;; (p (normalize (expand '(define (foo) (if 1 (define a 2))) default-env)))

(define default-env
  (letrec
      ((default-env
         '(()))

       (define-object
         (lambda (sym)
           (add-identifier! sym default-env)))

       (define-macro
         (lambda (sym expander)
           (let ((qid (define-object sym)))
             (add-macro! qid (expander default-env))))))

    (define-macro 'quote
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-quote (make-identifier 'quote builtin-environment)))
            `(,the-quote ,(second form))))))

    (define-macro 'define
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-define (make-identifier 'define builtin-environment))
                (the-lambda (make-identifier 'lambda mac-env)))
            (if (identifier? (second form))
                `(,the-define ,(second form) ,(third form))
                `(,the-define ,(car (second form))
                              (,the-lambda ,(cdr (second form))
                                           ,@(cddr form))))))))

    (define-macro 'lambda
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-lambda (make-identifier 'lambda builtin-environment))
                (the-begin (make-identifier 'begin mac-env)))
            `(,the-lambda ,(second form) (,the-begin . ,(cddr form)))))))

    (define-macro 'if
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-if (make-identifier 'if builtin-environment))
                (test (second form))
                (then (third form))
                (else (if (= (length form) 4) (fourth form) #f)))
            `(,the-if ,test ,then ,else)))))

    (define-macro 'begin
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-begin (make-identifier 'begin builtin-environment)))
            (cond
             ((= (length form) 1)
              #f)
             ((= (length form) 2)
              (second form))
             ((= (length form) 3)
              `(,the-begin ,(second form) ,(third form)))
             (else
              `(,the-begin ,(second form) (,(first form) . ,(cddr form)))))))))

    (define-macro 'set!
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-set! (make-identifier 'set! builtin-environment)))
            `(,the-set! ,(second form) ,(third form))))))

    (define-macro 'let
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-lambda (make-identifier 'lambda mac-env)))
            `((,the-lambda (,(second form))
                           . ,(cdddr form))
              ,(third form))))))

    (define-macro 'swap!
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-let (make-identifier 'let mac-env))
                (the-set! (make-identifier 'set! mac-env))
                (the-tmp (make-identifier 'tmp mac-env)))
            `(,the-let ,the-tmp ,(second form)
                       (,the-set! ,(second form) ,(third form))
                       (,the-set! ,(third form) ,the-tmp))))))

    (define-object '+)
    (define-object '-)
    (define-object '<)

    default-env))

(define test-form
  `(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1))
            (fib (- n 2))))))

(p (expand test-form default-env))

(p default-env)

(p (normalize (expand test-form default-env)))

(p (transform (normalize (expand test-form default-env)) (lambda (x) `(halt ,x))))

;; (p (emit (transform (normalize (expand test-form default-env)) (lambda (x) `(halt ,x)))))

(let ((v (emit (print (transform (normalize (expand test-form default-env)) (lambda (x) `(halt ,x)))))))
  (for-each print v))
