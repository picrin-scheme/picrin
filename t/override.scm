(import (picrin base)
        (picrin test))

(test-begin)

(define orig-cons cons)

(set! symbol? list)

(test '(1)
      (symbol? 1))

(test-end)
