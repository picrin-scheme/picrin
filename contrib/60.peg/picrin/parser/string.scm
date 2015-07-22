(define-library (picrin parser string)
  (import (except (scheme base) string)
          (picrin parser))
  (export string
          any-char
          eof
          parse-string)

  ;; string stream parser

  (define (string str)
    (lambda (i)
      (let ((i (car i)) (input (cdr i)))
        (let ((j (min (+ i (string-length str)) (string-length input))))
          (and (equal? str (string-copy input i j))
               `(,str . ,(cons j input)))))))

  (define any-char
    (lambda (i)
      (let ((i (car i)) (input (cdr i)))
        (and (< i (string-length input))
             `(,(string-ref input i) . ,(cons (+ i 1) input))))))

  (define eof
    (without any-char))

  (define (parse-string rule input)
    (parse rule (cons 0 input))))
