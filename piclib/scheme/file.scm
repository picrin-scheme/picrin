(define-library (scheme file)
  (import (scheme base))

  (define (call-with-input-file filename callback)
    (call-with-port (open-input-file filename) callback))

  (define (call-with-output-file filename callback)
    (call-with-port (open-output-file filename) callback))

  (export call-with-input-file
          call-with-output-file))
