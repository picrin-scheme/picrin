(define-library (picrin base)
  (import (picrin base core))

  (export define
          set!
          quote
          lambda
          if
          begin
          define-syntax))
