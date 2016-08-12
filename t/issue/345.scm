(import (scheme base)
        (picrin test))

(test-begin)

(define-syntax check1
  (syntax-rules ()
    ((_) 'hoge)))

(test 'hoge (check1))


(define-syntax check2
  (syntax-rules ()
    ((_ arg)
     (case arg
       ((OK) "OK")
       (else "NG")))))

(test "OK" (check2 'OK))

(test-end)
