(define-library (picrin base)
  (export attribute)

  (define attribute-table (make-attribute))

  (define (attribute obj)
    (or (attribute-table obj)
        (let ((dict (make-dictionary)))
          (attribute-table obj dict)
          dict))))
