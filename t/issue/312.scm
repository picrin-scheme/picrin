(import (scheme base)
        (picrin test))

(test-begin)
(test #f (equal? (make-list 10 1) (make-list 11 1)))

(let ((l1 (list 1 1))
      (l2 (list 1 1))
      (l3 (list 1 1 1)))
  (set-cdr! l1 l1)
  (set-cdr! l2 l2)
  (set-cdr! (cdr l3) l3)

  (test #f (equal? l1 (make-list 10 1)))
  (test #t (equal? l1 l1))
  (test #t (equal? l1 l2))
  (test #t (equal? l1 l3)))

(test-end)
