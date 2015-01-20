(import (scheme base)
        (scheme write)
        (picrin rb-tree)
        (picrin test))

(define tree (make-rb-tree))
(test (rb-tree-size tree) 0)
(test #t (rb-tree-insert tree 1))
(test #f (rb-tree-insert tree 1))

(test (rb-tree-size tree) 1)
(test #t (rb-tree-mem? tree 1))
(test #f (rb-tree-mem? tree 2))
(rb-tree-insert tree 2)
(test #t (rb-tree-mem? tree 1))
(test #t (rb-tree-mem? tree 2))

(test 1 (rb-tree-min tree))
(test 2 (rb-tree-max tree))

(rb-tree-insert tree 3)
(rb-tree-insert tree 4)
(rb-tree-insert tree 5)

(test 1 (rb-tree-min tree))
(test 5 (rb-tree-max tree))

(test #t (rb-tree-delete tree 1))
(test #f (rb-tree-mem? tree 1))
(test #f (rb-tree-delete tree 1))
