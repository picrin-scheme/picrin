(define-library (picrin optional)
  (import (scheme base))

  (define-syntax optional
    (syntax-rules ()
      ((_ args default)
       (let ((t args))
         (if (null? t) default (car t))))))

  (define-syntax let-optionals*
    (syntax-rules ()
      ((_ args () body ...)
       (begin body ...))
      ((_ args ((var default) . tail) body ...)
       (let* ((t args)
              (var (if (null? t) default (car t)))
              (remain (if (null? t) '() (cdr t))))
         (let-optionals* remain tail body ...)))
      ((_ args rest body ...)
       (let ((rest args))
         body ...))))

  (export optional
          let-optionals*))
