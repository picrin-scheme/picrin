(import (scheme base)
        (picrin test))

(test-begin)

(test "-0.1" (substring (number->string -0.1) 0 4))
(test "-0.9" (substring (number->string -0.9) 0 4))

(test (list :error: "string-copy: invalid index") (substring "foo" 2 5))
(test (list :error: "string-copy: invalid index") (substring "foo" -1 2))
(test (list :error: "string-copy: invalid index") (substring "foo" 2 -1))
(test (list :error: "string-copy: invalid index") (substring "foo" 7 7))
(test (list :error: "string-copy: invalid index") (substring "foo" 7 1))
(test (list :error: "string-copy: invalid index") (substring "foo" 3 1))

(test "oo" (substring "foo" 1 3))

(test-end)
