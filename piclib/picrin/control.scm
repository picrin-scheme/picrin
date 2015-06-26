(define-library (picrin control)
  (import (picrin base))

  (define escape call/cc)               ; create a new global variable slot

  (export escape))
