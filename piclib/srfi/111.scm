(define-library (srfi 111)
  (import (scheme base))

  (define-record-type <box>
    (box value)
    box?
    (value unbox set-box!))

  (export box box?
          unbox set-box!))
