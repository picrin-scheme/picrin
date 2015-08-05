(import (scheme base)
        (picrin test)
        (picrin big-number)
        (srfi 27))


; ->number
(test 192.0 (bigint->number (bigint-asl (make-bigint "12") 4)))
(test 384.0 (bigint->number (bigint-asl (make-bigint "3") 7)))

; underlying
(test #(2582) (bigint-underlying (make-bigint "2582")))


; addition
(define-syntax test-add
  (syntax-rules ()
    ((_ a b ab)
      (test #t (bigint-equal? (make-bigint ab) (bigint-add (make-bigint a) (make-bigint b)))))))
(test-add "215127315" "2385236125" "2600363440")
(test-add
  "6749346968279885792917645501676082665015680331686594141390629938583418409197715730606872803686797073"
   "153356901154961362321023710551178358916519555137945588510996804230108640808767023357542838211543493"
  "6902703869434847155238669212227261023932199886824539729901626742813527050006482753964415641898340566")
(test-add "4" "-2" "2")

; multiplication

(define-syntax test-mul
  (syntax-rules ()
    ((_ a b ab)
      (test #t (bigint-equal? (make-bigint ab) (bigint-mul (make-bigint a) (make-bigint b)))))))
(test-mul "15267" "41446" "632756082")
(test-mul "17568913159" "72371657891659178645" "1271491372671417140039272289555")

(test #t
  (bigint-equal? (make-bigint "18446744065119617025")
    (let ((ff32 (make-bigint "4294967295")))
    (bigint-mul ff32 ff32))))

; equality
(define-syntax test-equal?
  (syntax-rules ()
    ((_ a b)
      (test (equal? a b)
        (bigint-equal? (make-bigint a) (make-bigint b))))))

(test-equal? "12345" "12345")
(test-equal? "1" "4294967297")

; less
(define-syntax test-less?
  (syntax-rules ()
    ((_ a b ab)
      (test ab
        (bigint-less? (make-bigint a) (make-bigint b))))))
(test-less? "12" "11" #f)
(test-less? "-12" "-11" #t)
(test-less? "-0" "0" #f)
(test-less? "1" "4294967297" #t)


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
