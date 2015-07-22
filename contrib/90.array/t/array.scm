(import (scheme base)
        (scheme write)
        (picrin array)
        (picrin test))

(test-begin)

(define ary (make-array))

(array-push! ary 1)
(array-push! ary 2)
(array-push! ary 3)

(test 3 (array-pop! ary))
(test 2 (array-pop! ary))
(test 1 (array-pop! ary))

(array-unshift! ary 1)
(array-unshift! ary 2)
(array-unshift! ary 3)

(test 3 (array-shift! ary))
(test 2 (array-shift! ary))
(test 1 (array-shift! ary))

(test-end)
