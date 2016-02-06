(define-library (srfi 17)

  (import (except (scheme base) set!)
          (prefix (only (scheme base) set!) %)
          (except (picrin base) set!)
          (srfi 1)
          (srfi 8))

  (define-syntax set!
    (syntax-rules ()
      ((_ (proc args ...) val)
       ((setter proc) args ... val))
      ((_ var val)
       (%set! var val))))

  (define setter
    (letrec ((setter
              (lambda (proc)
                (let ((setter (dictionary-ref (attribute proc) '@@setter)))
                  (if setter
                      (cdr setter)
                      (error "no setter found")))))
             (set-setter!
              (lambda (proc setter)
                (dictionary-set! (attribute proc) '@@setter setter))))
      (set-setter! setter set-setter!)
      setter))

  (define (getter-with-setter get set)
    (let ((proc (lambda args (apply get args))))
      (set! (setter proc) set)
      proc))

  (set! (setter car) set-car!)
  (set! (setter cdr) set-cdr!)
  (set! (setter vector-ref) vector-set!)
  (set! (setter string-ref) string-set!)
  (set! (setter bytevector-u8-ref) bytevector-u8-set!)
  (set! (setter list-ref) list-set!)

  (export set!
          setter
          getter-with-setter))
