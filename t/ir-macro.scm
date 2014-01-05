(import (scheme base)
        (picrin macro))

(define-syntax aif
  (ir-macro-transformer
   (lambda (form inject cmp)
     (let ((it (inject 'it))
           (expr (car (cdr form)))
           (then (car (cdr (cdr form))))
           (else (car (cdr (cdr (cdr form))))))
       `(let ((,it ,expr))
          (if ,it ,then ,else))))))

(aif (member 'b '(a b c)) (car it) #f)

;;; test hygiene begin

(define-syntax mif
  (ir-macro-transformer
   (lambda (form inject cmp)
     (let ((expr (car (cdr form)))
           (then (car (cdr (cdr form))))
           (else (car (cdr (cdr (cdr form))))))
       `(let ((it ,expr))
          (if it ,then ,else))))))

(let ((if 42))
  (mif 1 2 3))
; => 2

(let ((it 42))
  (mif 1 it 2))
; => 42

;;; end



;;; test core syntax begin

(mif 'a 'b 'c)
; => b

(define-syntax loop
  (ir-macro-transformer
   (lambda (expr inject cmp)
     (let ((body (cdr expr)))
       `(call-with-current-continuation
         (lambda (,(inject 'exit))
           (let f ()
             ,@body (f))))))))

(define a 1)
(loop
 (if (= a 2) (exit #f))
 (set! a 2))
; => #f

(loop
 (define a 1)
 (if (= a 1) (exit #f)))
; => #f

;;; end
