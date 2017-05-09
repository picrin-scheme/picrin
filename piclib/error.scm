(begin

  (define current-exception-handlers
    (let ((e error))
      (make-parameter (list e))))

  (define (raise x)
    (let ((handlers (current-exception-handlers)))
      (parameterize ((current-exception-handlers (cdr handlers)))
        ((car handlers) x)
        (error "handler returned" x))))

  (define (raise-continuable x)
    (let ((handlers (current-exception-handlers)))
      (parameterize ((current-exception-handlers (cdr handlers)))
        ((car handlers) x))))

  (define (with-exception-handler handler thunk)
    (let ((handlers (current-exception-handlers)))
      (parameterize ((current-exception-handlers (cons handler handlers)))
        (thunk))))

  (define-record-type error-object
      (make-error-object type message irritants)
      error-object?
    (type error-object-type)
    (message error-object-message)
    (irritants error-object-irritants))

  (set! error
        (lambda (message . irritants)
          (raise (make-error-object #f message irritants))))

  (set! display
        (let ((d display))
          (lambda (x . port)
            (let ((port (if (null? port) (current-error-port) (car port))))
              (if (error-object? x)
                  (let ()
                    (when (error-object-type x)
                      (d (error-object-type x) port)
                      (d "-" port))
                    (d "error: \"" port)
                    (d (error-object-message x) port)
                    (d "\"")
                    (for-each
                     (lambda (x)
                       (d " " port)
                       (write x port))
                     (error-object-irritants x))
                    (d "\n" port))
                  (d x port)))))))
