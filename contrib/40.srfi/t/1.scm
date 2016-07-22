;; Certain portions of this document -- the specific, marked segments of text
;; describing the R5RS procedures -- were adapted with permission from the R5RS
;; report.
;; 
;; All other text is copyright (C) Olin Shivers (1998, 1999). All Rights
;; Reserved.
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE. 

(import (scheme base)
        (srfi 1)
        (picrin test))

(test-begin)

(test '(a) (cons 'a '()))
(test '((a) b c d) (cons '(a) '(b c d)))
(test '("a" b c) (cons "a" '(b c)))
(test '(a . 3) (cons 'a 3))
(test '((a b) . c) (cons '(a b) 'c))

(test '(a 7 c) (list 'a (+ 3 4) 'c))
(test '() (list))

(test '(a b c) (xcons '(b c) 'a))

(test '(1 2 3 . 4) (cons* 1 2 3 4))
(test 1 (cons* 1))

(test '(c c c c) (make-list 4 'c))

(test '(0 1 2 3) (list-tabulate 4 values))

; TODO: Test list-copy

; (test #0=(z q . #0#) (circular-list 'z 'q))

(test '(0 1 2 3 4) (iota 5))
(test '(0 -0.1 -0.2 -0.3 -0.4) (iota 5 0 -0.1))

; TODO: Test proper-list?
; TODO: Test circular-list?
; TODO: Test dotted-list?

(test #t (pair? '(a . b)))
(test #t (pair? '(a b c)))
(test #f (pair? '()))
(test #f (pair? '#(a b)))
(test #f (pair? 7))
(test #f (pair? 'a))

; TODO: Test null?
; TODO: Test null-list?
; TODO: Test not-pair?

(test #t (list= eq?))
(test #t (list= eq? '(a)))
; TODO: Add non-trivial test cases for list=

(test 'a (car '(a b c)))
(test '(a) (car '((a) b c d)))
(test 1 (car '(1 . 2)))
(test #t (error-object?
           (guard (exn (else exn))
             (car '()))))

(test '(b c) (cdr '(a b c)))
(test '(b c d) (cdr '((a) b c d)))
(test 2 (cdr '(1 . 2)))
(test #t (error-object?
           (guard (exn (else exn))
             (cdr '()))))

; TODO: Test /c[ad]{2,4}r/

(test 'c (list-ref '(a b c d) 2))

; TODO: Test first, second, ..., tenth
; TODO: Test car+cdr

(test '(a b) (take '(a b c d e) 2))
(test '(1 2) (take '(1 2 3 . d) 2))
(test '(1 2 3) (take '(1 2 3 . d) 3))

(test '(c d e) (drop '(a b c d e) 2))
(test '(3 . d) (drop '(1 2 3 . d) 2))
(test 'd (drop '(1 2 3 . d) 3))

(test '(d e) (take-right '(a b c d e) 2))
; (test '(2 3 . d) (take-right '(1 2 3 . d) 2))
; (test 'd (take-right '(1 2 3 . d) 0)

(test '(a b c) (drop-right '(a b c d e) 2))
; (test '(1) (drop-right '(1 2 3 . d) 2))
; (test '(1 2 3) (drop-right '(1 2 3 . d) 0))

(test '(1 3) (take! (circular-list 1 3 5) 8)) ; implementation dependent behavior

; TODO: Test split-at
; TODO: Test split-at!

(test 'c (last '(a b c)))
(test '(c) (last-pair '(a b c)))

; TODO: Test length
; TODO: Test length+

(test '(x y) (append '(x) '(y)))
(test '(a b c d) (append '(a) '(b c d)))
(test '(a (b) (c)) (append '(a (b)) '((c))))
(test '(a b c . d) (append '(a b) '(c . d)))
(test 'a (append '() 'a))
(test '(x y) (append '(x y)))
(test '() (append))

; TODO: Test append!
; TODO: Test concatenate
; TODO: Test concatenate!

(test '(c b a) (reverse '(a b c)))
(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

; TODO: Test reverse!
; TODO: Test append-reverse
; TODO: Test append-reverse!

(test '((one 1 odd) (two 2 even) (three 3 odd))
      (zip '(one two three) 
           '(1 2 3)
           '(odd even odd even odd even odd even)))
(test '((1) (2) (3)) (zip '(1 2 3)))
(test '((3 #f) (1 #t) (4 #f) (1 #t)) (zip '(3 1 4 1) (circular-list #f #t)))

; TODO: Test /unzip[1-5]/

(test 3 (count even? '(3 1 4 1 5 9 2 5 6)))
(test 3 (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)))
(test 2 (count < '(3 1 4 1) (circular-list 1 10)))

; TODO: Test fold
; TODO: Test fold-right
; TODO: Test pair-fold

(test '((a b c) (b c) (c)) (pair-fold-right cons '() '(a b c)))

; TODO: Test reduce
; TODO: Test reduce-right
; TODO: Test unfold
; TODO: unfold-right

(test '(b e h) (map cadr '((a b) (d e) (g h))))
(test '(1 4 27 256 3125) (map (lambda (n) (expt n n))
                              '(1 2 3 4 5)))
(test '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(test '(4 1 5 1) (map + '(3 1 4 1) (circular-list 1 0)))

(test #(0 1 4 9 16)
      (let ((v (make-vector 5)))
        (for-each (lambda (i)
                    (vector-set! v i (* i i)))
                  '(0 1 2 3 4))
        v))

; TODO: Test append-map
; TODO: Test append-map!
; TODO: Test map!
; TODO: Test map-in-order
; TODO: Test pair-for-each

(test '(1 9 49) (filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7)))

(test '(0 8 8 -4) (filter even? '(0 7 8 8 43 -4)))

; TODO: Test partition

(test '(7 43) (remove even? '(0 7 8 8 43 -4)))

; TODO: Test filter!
; TODO: Test partition!
; TODO: Test remove!

(test 4 (find even? '(3 1 4 1 5 9)))

(test '(-8 -5 0 0) (find-tail even? '(3 1 37 -8 -5 0 0)))
(test #f (find-tail even? '(3 1 37 -5)))

(test '(2 18) (take-while even? '(2 18 3 10 22 9)))

; TODO: Test take-while!

(test '(3 10 22 9) (drop-while even? '(2 18 3 10 22 9)))

; TODO: Test span
; TODO: Test span!
; TODO: Test break
; TODO: Test break!

(test #t (any integer? '(a 3 b 2.7)))
(test #f (any integer? '(a 3.1 b 2.7)))
(test #t (any < '(3 1 4 1 5) '(2 7 1 8 2)))

; TODO: Test every

(test 2 (list-index even? '(3 1 4 1 5 9)))
; (test 1 (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))
; (test #f (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))

(test '((a) c) (member (list 'a) '(b (a) c)))

(test '(a b c) (memq 'a '(a b c)))
(test '(b c) (memq 'b '(a b c)))
(test #f (memq 'a '(b c d)))
(test #f (memq (list 'a) '(b (a) c)))

(test '(101 102) (memv 101 '(100 101 102)))

; TODO: Test delete
; TODO: Test delete!

(test '(a b c z) (delete-duplicates '(a b a c a b c z)))
(test '((a . 3) (b . 7) (c . 1))
      (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                         (lambda (x y) (eq? (car x) (car y)))))

; TODO: Test delete-duplicates!

(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(define e '((a 1) (b 2) (c 3)))
(test '(a 1) (assq 'a e))
(test '(b 2) (assq 'b e))
(test #f (assq 'd e))
(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

; TODO: Test alist-cons
; TODO: Test alist-copy
; TODO: Test alist-delete
; TODO: Test alist-delete!

(test #t (lset<= eq? '(a) '(a b a) '(a b c c)))
(test #t (lset<= eq?))
(test #t (lset<= eq? '(a)))

(test #t (lset= eq? '(b e a) '(a e b) '(e e b a)))
(test #t (lset= eq?))
(test #t (lset= eq? '(a)))

(test '(u o i a b c d c e) (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) lset=)

(test '(u o i a b c d e) (lset-union eq? '(a b c d e) '(a e i o u)) lset=)
(test '(x a a c) (lset-union eq? '(a a c) '(x a x)) lset=)
(test '() (lset-union eq?) lset=)
(test '(a b c) (lset-union eq? '(a b c)) lset=)

(test '(a e) (lset-intersection eq? '(a b c d e) '(a e i o u)) lset=)
(test '(a x a) (lset-intersection eq? '(a x y a) '(x a x z)) lset=)
(test '(a b c) (lset-intersection eq? '(a b c)) lset=)

(test '(b c d) (lset-difference eq? '(a b c d e) '(a e i o u)) lset=)
(test '(a b c) (lset-difference eq? '(a b c)) lset=)

(test '(d c b i o u) (lset-xor eq? '(a b c d e) '(a e i o u)) lset=)
(test '() (lset-xor eq?) lset=)
(test '(a b c d e) (lset-xor eq? '(a b c d e)) lset=)

; TODO: Test lset-diff+intersection
; TODO: Test lset-union!
; TODO: Test lset-intersection!
; TODO: Test lset-difference!
; TODO: Test lset-xor!
; TODO: Test lset-diff+intersection!
; TODO: Test set-car!
; TODO: Test set-cdr!

(test-end)
