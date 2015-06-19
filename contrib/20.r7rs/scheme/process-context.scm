(define-library (scheme process-context)
  (import (picrin base))

  (export command-line
          emergency-exit
          exit
          get-environment-variable
          get-environment-variables))
