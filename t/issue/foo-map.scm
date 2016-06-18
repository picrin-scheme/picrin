(import (scheme base)
        (picrin test))

(test-begin)

(define (char-inc c)
  (integer->char (+ (char->integer c) 1)))

(define (char-dec c)
  (integer->char (- (char->integer c) 1)))

(test "tsvcmxdmqr"
 (string-map (lambda (c k)
               ((if (eqv? k #\+) char-inc char-dec) c))
             "studlycnps xxx"
             "+-+-+-+-+-"))

(test "abcdefgh"
      (begin
        (define s "")
        (string-for-each
         (lambda (a b)
           (set! s (string-append s (string a b))))
         "aceg hij"
         "bdfh")
        s))

(test #(#(1 6 9) #(2 7 10) #(3 8 11))
      (vector-map vector #(1 2 3 4 5) #(6 7 8) #(9 10 11 12)))

(test "(1 4 1)(2 5 1)"
      (call-with-port (open-output-string)
        (lambda (port)
          (parameterize ((current-output-port port))
            (vector-for-each
             (lambda args (display args))
             #(1 2 3)
             #(4 5)
             #(1 1))
            (get-output-string port)))))

(test-end)
