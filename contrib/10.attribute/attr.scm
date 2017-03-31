(define-library (picrin base)

  (define attribute-table (make-ephemeron-table))

  (define (attribute obj)
    (let ((r (attribute-table obj)))
      (if r
          (cdr r)
          (let ((dict (make-dictionary)))
            (attribute-table obj dict)
            dict))))

  (export attribute))
