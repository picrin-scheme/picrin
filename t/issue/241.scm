(import (scheme base)
        (scheme write)
        (picrin test))

(define-syntax hoo
  (er-macro-transformer
   (lambda (form r cmp)
     (if (cmp (r 'hoge) (cadr form))
         "You got"
         `',(cadr form)))))

(test "You got" (hoo hoge))
