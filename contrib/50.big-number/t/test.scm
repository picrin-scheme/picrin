(import (scheme base)
        (picrin test)
        (picrin big-number)
        (srfi 27))

(define (test-add x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (+ x y)))
      (bigint-underlying (bigint-add a b)))))

(define (test-sub x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (- x y)))
      (bigint-underlying (bigint-sub a b)))))

(define (test-mul x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
      (bigint-underlying (make-bigint (* x y)))
      (bigint-underlying (bigint-mul a b)))))

(define (bi-add x y)
  (bigint-underlying
    (bigint-add
      (make-bigint x)
      (make-bigint y))))

(define (test-equal x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test
        (equal? x y)
        (bigint-equal? a b))))

(define (test-bigint->number x y)
  (let ((a (make-bigint x)) (b (make-bigint y)))
    (test (+ x y)
      (bigint->number (bigint-add a b)))))


(define (rand-int n) (floor (* n (random-real))))
(define (test-random func time)
  (do ((iteration 0 (+ iteration 1)))
    ((>= iteration time) #t)
    (let ((x (rand-int 32535)) (y (rand-int 32535)))
      (func x y))))

(test-random test-add 3)
(test-random test-equal 3)
(test-random test-bigint->number 3)
(test-random test-mul 3)


(test 192.0 (bigint->number (bigint-asl (make-bigint "12") 4)))
(test 384.0 (bigint->number (bigint-asl (make-bigint "3") 7)))


(test #(2582) (bigint-underlying (make-bigint "2582")))
(test #f (bigint-less? (make-bigint "12") (make-bigint "11")))
(test #t (bigint-less? (make-bigint "-12") (make-bigint "-11")))
(test #f (bigint-less? (make-bigint "-0") (make-bigint "0")))


; addition
(test #t
  (bigint-equal? (make-bigint "2600363440")
    (bigint-add (make-bigint "215127315") (make-bigint "2385236125"))))
(test (bigint-underlying (make-bigint "6902703869434847155238669212227261023932199886824539729901626742813527050006482753964415641898340566"))
    (bigint-underlying (bigint-add (make-bigint "6749346968279885792917645501676082665015680331686594141390629938583418409197715730606872803686797073")
      (make-bigint "153356901154961362321023710551178358916519555137945588510996804230108640808767023357542838211543493"))))


; multiplication

(test #t (bigint-equal? (make-bigint "632756082") (bigint-mul (make-bigint "15267") (make-bigint "41446"))))
(test #t
  (bigint-equal? (make-bigint "1271491372671417140039272289555")
    (bigint-mul (make-bigint "17568913159") (make-bigint "72371657891659178645"))))

(test #t
  (bigint-equal? (make-bigint "18446744065119617025")
    (let ((ff32 (make-bigint "4294967295")))
    (bigint-mul ff32 ff32))))

; factorial

(define (fact-big n)
  (let loop ((acc (make-bigint 1)) (m n))
    (if (= m 0) acc
      (loop (bigint-mul acc m) (- m 1)))))

(test #t
  (bigint-equal? 3628800 (fact-big 10)))

(test #t
  (bigint-equal? (make-bigint "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000")
  (fact-big 100)))
