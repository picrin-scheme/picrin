(import (scheme base)
        (picrin test))

(test-begin "syntax-rules")

(define-syntax extract?
  (syntax-rules ()
    ((_ symb body _cont-t _cont-f)
     (letrec-syntax
         ((tr
            (syntax-rules (symb)
              ((_ x symb tail (cont-head symb-l . cont-args) cont-false)
               (cont-head (x . symb-l) . cont-args))
            ((_ d (x . y) tail . rest) ; if body is a composite form,
             (tr x x (y . tail) . rest)) ; look inside
            ((_ d1 d2 () cont-t (cont-head symb-l . cont-args))
             (cont-head (symb . symb-l) . cont-args))
            ((_ d1 d2 (x . y) . rest)
             (tr x x y . rest)))))
       (tr body body () _cont-t _cont-f)))))

(define-syntax extract
  (syntax-rules ()
    ((_ symb body cont)
     (extract? symb body cont cont))))

(define-syntax mbi-dirty-v1
  (syntax-rules ()
    ((_ _val _body)
     (let-syntax
         ((cont
           (syntax-rules ()
             ((_ (symb) val body)
              (let ((symb val)) body)))))
       (extract i _body (cont () _val _body))))))

(test 11 (mbi-dirty-v1 10 (+ i 1)))

(test-end)
