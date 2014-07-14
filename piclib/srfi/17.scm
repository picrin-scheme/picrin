(define-library (srfi 17)

  (define-library (rename set!)
    (import (scheme base))
    (export (rename set! %set!)
            define
            quasiquote
            letrec
            let
            error
            apply
            define-syntax
            syntax-rules
            lambda
            if
            quote
            begin
            vector-ref
            string-ref
            bytevector-u8-ref
            vector-set!
            string-set!
            bytevector-u8-set!
            list-set!))
  
  (import (rename set!)
          (srfi 1))
  
  (define-syntax set!
    (syntax-rules ()
      ((_ (proc args ...) val)
       ((setter proc) args ... val))
      ((_ var val)
       (%set! var val))))

  (define setter-alist '())

  (define setter
    (letrec ((setter
              (lambda (proc)
                (let ((probe (assv proc setter-alist)))
                  (if probe
                      (cdr probe)
                      (error "No setter for " proc)))))
             (set-setter!
              (lambda (proc setter)
                (set! setter-alist
                      (alist-cons proc setter setter-alist)))))
      (set-setter! setter set-setter!)
      (set-setter! car set-car!)
      (set-setter! cdr set-cdr!)
      (set-setter! caar (lambda (p v) (set-car! (car p) v)))
      (set-setter! cadr (lambda (p v) (set-car! (cdr p) v)))
      (set-setter! cdar (lambda (p v) (set-cdr! (car p) v)))
      (set-setter! cddr (lambda (p v) (set-cdr! (cdr p) v)))
      (set-setter! vector-ref vector-set!)
      (set-setter! string-ref string-set!)
      (set-setter! bytevector-u8-ref bytevector-u8-set!)
      (set-setter! list-ref list-set!)
      setter))

  (define (getter-with-setter get set)
    (let ((proc (lambda args (apply get args))))
      (set! (setter proc) set)
      proc))

  (export set!
          setter
          getter-with-setter
          setter-alist))
