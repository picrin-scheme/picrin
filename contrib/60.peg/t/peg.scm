;;; test case

(import (scheme base)
        (picrin test)
        (picrin procedure)
        (picrin parser)
        (picrin parser string))

(test-begin "(picrin parser) and (picrin parser string)")

(define LPAREN (string "("))
(define RPAREN (string ")"))

(define PLUS (string "+"))
(define MINUS (string "-"))

(define ONE (fapply (constant 1) (string "1")))

(define S (lazy
           (fapply (>> list car) A eof)))

(define A (lazy
           (choice
            (fapply (lambda (p _ a) (list '+ p a)) P PLUS A)
            (fapply (lambda (p _ a) (list '- p a)) P MINUS A)
            P)))

(define P (lazy
           (choice
            (between LPAREN A RPAREN)
            ONE)))

(define-syntax test-success
  (syntax-rules ()
    ((_ expect str)
     (test (cons expect (cons (string-length str) str))
           (parse-string S str)))))

(test-success 1 "(1)")
(test-success '(- (+ 1 1) 1) "((1+1)-1)")
(test-success '(- (+ 1 1) 1) "((1+(1))-1)")
(test-success '(+ 1 (- 1 (+ 1 (- 1 (+ 1 1))))) "(1+(1-(1+(1-(1+1)))))")
(test-success '(+ 1 (+ 1(- 1 (+ 1 (- 1 (+ 1 1)))))) "(1+1+(1-(1+(1-(1+1)))))")

(test-end)
