(import (scheme base)
        (picrin control list)
        (picrin test))

(test '(1 2 3)
      (for
       (yield (in '(1 2 3)))))

(test '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c))
      (for
       (let ((n (in '(1 2 3)))
             (c (in '(a b c))))
         (yield (list n c)))))
