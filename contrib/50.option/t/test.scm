(import (picrin base)
        (picrin test)
        (picrin control option))

(define phonebook
  '(("Bob" . "01788 665242")
    ("Fred" . "01624 556442")
    ("Alice" . "01889 985333")
    ("Jane" . "01732 187565")))

(define nums
  '((one . 1) (two . 2) (three . 3) (four . 19)))

(define num-dict
  (alist->dictionary nums))

(test '("01889 985333" . 3)
 (reify
  (let* ((a (reflect (assoc "Alice" phonebook)))
         (b (reflect (dictionary-ref num-dict 'three))))
    (cons (cdr a) (cdr b)))))

(test '#f
 (reify
  (let* ((a (reflect (assoc "Alice" phonebook)))
         (b (reflect (dictionary-ref num-dict 'five))))
    (cons (cdr a) (cdr b)))))
