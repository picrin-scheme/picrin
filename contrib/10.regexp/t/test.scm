(import (scheme base)
        (picrin test)
        (picrin regexp))

(test #t (regexp? (regexp "simple")))
(test #f (regexp? "it\\s[s]e+ms\\s(reg)?exp"))
(test-values '(("abcd") ("b")) (regexp-match (regexp "a(b)cd") "abdacabcd"))
(test '("a" "b" "c" "d") (regexp-split (regexp ",") "a,b,c,d"))
(test '("a" "b" "c" "d") (regexp-split (regexp "\.+") "a.b....c.....d"))
(test "newline tab spase " (regexp-replace (regexp "\\s") " " "newline
tab	space "))
