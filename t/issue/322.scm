(import (scheme base)
        (picrin test))

(test-begin)

(test #f (string->number "12e43r"))

(test #f (string->number "12e+43r"))

(test #f (string->number "12e+43e54"))

(test #f (string->number "12e+"))

(test #f (string->number "12e"))

(test #f (string->number "+12e"))

(test #f (string->number "-12e"))

(test -12 (string->number "-12"))

(test -12.0 (string->number "-12.0"))

(test #f (string->number "-12.0e-5t"))

(test-end)
