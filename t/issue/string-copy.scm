(import (scheme base)
        (picrin test))

(test-begin)

(test "456" (string-copy (string-copy "1234567" 3) 0 3))

(test-end)
