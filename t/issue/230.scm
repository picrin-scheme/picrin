(import (scheme base)
        (picrin test))

(define-record-type foo
  (make-foo a)
  foo?
  (b get-a))

(test 10 (get-a (make-foo 10)))
