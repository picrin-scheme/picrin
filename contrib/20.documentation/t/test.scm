(import (scheme base)
        (picrin documentation)
        (srfi 17)
        (picrin test))

(define (hoge) ())
(test #f (documentation hoge))

(set! (documentation hoge) "egoh")

(test "egoh" (documentation hoge))

(define foo
  (lambda ()
    "ya foooo"
    #f))
(test #f (foo))
(test "ya foooo" (documentation foo))

(define (bar)
  "Yo"
  #f)
(test #f (bar))
(test "Yo" (documentation bar))

(define foo2
  (lambda () #f))
(test #f (foo2))
(test #f (documentation foo2))

(define (bar2)
  #f)
(test #f (bar2))
(test #f (documentation bar2))
