(import (srfi 27)
        (scheme base)
        (picrin test))

(test #t (procedure? random-real))
