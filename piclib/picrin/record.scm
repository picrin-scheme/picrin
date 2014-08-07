(define-library (picrin record)
  (import (scheme base))

  (define (define-record-writer* record-type writer)
    (record-set! record-type 'writer writer))

  (define-syntax define-record-writer
    (syntax-rules ()
      ((_ (type obj) body ...)
       (define-record-writer* type
         (lambda (obj)
           body ...)))
      ((_ type writer)
       (define-record-writer* type
         writer))))

  (export define-record-writer))
