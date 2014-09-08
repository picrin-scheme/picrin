(define-library (picrin record)
  (import (picrin base)
          (scheme base))

  (define (set-record-writer! record-type writer)
    (record-set! record-type 'writer writer))

  (define-syntax define-record-writer
    (syntax-rules ()
      ((_ (type obj) body ...)
       (set-record-writer! type
         (lambda (obj)
           body ...)))
      ((_ type writer)
       (set-record-writer! type
         writer))))

  (export define-record-writer))
