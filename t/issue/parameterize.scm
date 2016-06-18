(import (scheme base)
        (picrin test))

(test-begin)

(define a #f)

(parameterize ()
  (set! a (make-parameter 1)))

(test 1 (a))

(test-end)
