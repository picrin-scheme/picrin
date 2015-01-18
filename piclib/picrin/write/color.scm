(define-library (picrin write color)
  (import (picrin base))


  (define (call-with-colored-output color proc)
    (display (case color
               ((black)   "[0;30m")
               ((red)     "[0;31m")
               ((green)   "[0;32m")
               ((yellow)  "[0;33m")
               ((blue)    "[0;34m")
               ((magenta) "[0;35m")
               ((cyan)    "[0;36m")
               ((white)   "[0;37m")
               (else => (lambda (color) (error "unknown color ~a" color)))))
    (proc)
    (display "[0;39m"))

  (export call-with-colored-output)

  )

