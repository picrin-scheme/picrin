(define-library (scheme file)
  (import (picrin base)
          (scheme base))

  (define (call-with-input-file filename callback)
    (call-with-port (open-input-file filename) callback))

  (define (call-with-output-file filename callback)
    (call-with-port (open-output-file filename) callback))

  (define (with-input-from-file filename thunk)
    (call-with-input-file filename
      (lambda (port)
        (parameterize ((current-input-port port))
          (thunk)))))

  (define (with-output-to-file filename thunk)
    (call-with-output-file filename
      (lambda (port)
        (parameterize ((current-output-port port))
          (thunk)))))

  (export open-input-file
          open-binary-input-file
          open-output-file
          open-binary-output-file
          delete-file
          file-exists?
          call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file))
