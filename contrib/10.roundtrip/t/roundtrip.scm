(import (scheme base)
        (srfi 27)
        (scheme inexact)
        (picrin test))

(test-begin)

(define (rountrip-ok number)
  (let ((radix 10)) 
    (eqv? number (string->number (number->string number radix) radix))))

(test #t (rountrip-ok -nan.0))

(test #t (rountrip-ok +nan.0))

(test #t (rountrip-ok -inf.0))

(test #t (rountrip-ok +inf.0))

(test #t (rountrip-ok +0.0))

(test #t (rountrip-ok -0.0))

(test #t (rountrip-ok 0.0))

(test -inf.0 (string->number "-inf.0"))

(test +inf.0 (string->number "+inf.0"))

(test #t (nan? (string->number "-nan.0")))

(test #t (nan? (string->number "+nan.0")))

(define (random-roundtrip)
  (let ((r (random-real)))
    (if (rountrip-ok r)
      #t
      r)))

(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))
(test #t (random-roundtrip))

(test-end)
