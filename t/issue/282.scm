(import (scheme base)
        (picrin test))

(test-begin)

(test "-0.1" (substring (number->string -0.1) 0 4))
(test "-0.9" (substring (number->string -0.9) 0 4))
(test "-1.0" (substring (number->string -1.0) 0 4))
(test "-1.1" (substring (number->string -1.1) 0 4))

(test-end)
