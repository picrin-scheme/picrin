(define-library (picrin symbol)
  (import (picrin base symbol))

  (export symbol?
          symbol=?
          symbol->string
          string->symbol))
