;  the default repl environment

(define-library (picrin user)
  (import (scheme base)
          (scheme load)
          (scheme process-context)
          (scheme read)
          (scheme write)
          (scheme file)
          (scheme inexact)
          (scheme cxr)
          (scheme lazy)
          (scheme time)
          (picrin macro)))
