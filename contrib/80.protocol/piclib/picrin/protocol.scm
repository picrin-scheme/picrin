(define-library (picrin protocol)
  (import (scheme base)
          (srfi 1))

  (import (picrin class))

  (define method-table
    '())

  (define (applicative? args types)
    (cond
     ((and (null? args) (null? types))
      #true)
     ((and (pair? args) (pair? types))
      (and (instance? (car args) (car types)) (applicative? (cdr args) (cdr types))))
     (else
      #false)))

  (define (find-generic generic)
    (or (assq generic method-table)
        (error "no method alist found")))

  (define (find-method generic args)
    (let ((methods (cdr (find-generic generic))))
      (let ((m (filter (lambda (x) (applicative? args (cdr x))) methods)))
        (if (null? m)
            #f
            (car (car m))))))

  (define (add-generic generic)
    (set! method-table (cons (cons generic '()) method-table)))

  (define (add-method generic method types)
    (let ((r (find-generic generic)))
      (set-cdr! r (cons (cons method types) (cdr r)))))

  (define (add-methods methods prototypes)
    (for-each
     (lambda (method prototype)
       (add-method (car prototype) method (cdr prototype)))
     methods
     prototypes))

  (define make-generic
    (lambda ()
      (letrec ((self (lambda args
                       (let ((m (find-method self args)))
                         (if m
                             (apply m args)
                             (error "method not found"))))))
        (add-generic self)
        self)))

  (define-syntax define-protocol
    (syntax-rules ()
      ((define-protocol (name type ...) (method arg ...) ...)
       (begin
         (define method
           (make-generic))
         ...
         (define name
           (lambda (type ...)
             (lambda methods
               (add-methods methods (list (list method arg ...) ...)))))))))

  (define-syntax define-instance
    (syntax-rules ()
      ((define-instance (name arg ...) method ...)
       ((name arg ...) method ...))))

  (export define-protocol
          define-instance))
