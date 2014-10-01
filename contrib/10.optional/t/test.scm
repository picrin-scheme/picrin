(import (scheme base)
        (picrin optional)
        (picrin test))

(test 0 (optional '() 0))
(test 1 (optional '(1) 0))

(test '(0 0) (let-optionals* '() ((a 0) (b 0)) (list a b)))
(test '(1 0) (let-optionals* '(1) ((a 0) (b 0)) (list a b)))
(test '(1 2) (let-optionals* '(1 2) ((a 0) (b 0)) (list a b)))
(test '(1 1) (let-optionals* '(1) ((a 0) (b a)) (list a b)))

(test '(0 ()) (let-optionals* '() ((a 0) . r) (list a r)))
(test '(1 ()) (let-optionals* '(1) ((a 0) . r) (list a r)))
(test '(1 (2)) (let-optionals* '(1 2) ((a 0) . r) (list a r)))
