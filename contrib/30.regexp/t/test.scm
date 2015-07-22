(import (scheme base)
        (picrin test)
        (picrin regexp))

(test #t (regexp? (regexp "simple")))
(test #f (regexp? "it\\s[s]e+ms\\s(reg)?exp"))
 (test-values (values '("abcd" "b") '(5 6)) (regexp-match (regexp "a(b)cd") "abdacabcd"))
(test '("a" "b" "c" "d") (regexp-split (regexp ",") "a,b,c,d"))
(test '("a" "b" "c" "d") (regexp-split (regexp "\\.+") "a.b....c.....d"))
(test "a b c d" (regexp-replace (regexp ",") "a,b,c,d" " "))
(test "newline tab space " (regexp-replace (regexp "[\n\t ]") "newline
tab	space " " "))
