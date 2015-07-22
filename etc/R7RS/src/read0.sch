;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for R6RS get-datum.
;;; Uses R6RS exception handlers to recover from lexical errors.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (rnrs base)
        (rnrs unicode)
        (rnrs bytevectors)
        (rnrs lists)
        (rnrs control)
        (rnrs exceptions)
        (rnrs conditions)
        (rnrs io ports)
        (rnrs io simple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Test framework.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Change to #t if you want to test extensions.

(define *test-extensions* #f)

; Implementation-specific flag that enables extensions.

(define *extensions-enabler* "#!larceny")

; Help procedures (mainly for using a non-R6RS reader to construct test cases)

(define (teststring . args)
  (define (loop revargs chars)
    (if (null? revargs)
        (list->string chars)
        (let ((x (car revargs))
              (revargs (cdr revargs)))
          (cond ((char? x)
                 (loop revargs (cons x chars)))
                ((string? x)
                 (loop revargs (append (string->list x) chars)))
                ((integer? x)
                 (loop revargs (cons (integer->char x) chars)))
                (else
                 (assert #f))))))
  (loop (reverse args) '()))

; Given a string, returns the list of results parsed from the string.

(define (testparse s)
  (let ((in (open-string-input-port s)))
    (do ((x (get-datum in) (get-datum in))
         (results '() (cons x results)))
        ((eof-object? x)
         (reverse results)))))

; The failed inputs and their expected outcomes are recorded here
; as an aid to debugging.

(define test-input)
(define failed-inputs '())
(define failed-expected '())

; Given an input string and a description of the expected results,
; calls testparse on the input string and compares with the expected.
; The description of the expected results is one of:
;
;     a list of expected results that can be compared using equal?
;     a unary predicate that is true of the expected results
;     the symbol error (meaning get-datum should raise an exception)
;
; Tests that should fail are preceded by #!r6rs, in an attempt
; to disable implementation-specific extensions to the lexical
; syntax that might otherwise be allowed by default.

(define (dotest s expected)
  (set! test-input s)
  (cond ((eq? 'error expected)
         (let ((s (string-append "#!r6rs " s)))
           (mustfail s testparse s)))
        ((procedure? expected)
         (let ((results (safely (lambda () (testparse s))
                                '(this should not happen))))
           (if (expected results)
               #t
               (failure s expected))))
        (else
         (let ((results (safely (lambda () (testparse s))
                                '(this should not happen))))
           (if (equal? results expected)
               #t
               (failure s expected))))))

(define (dotest-extension s expected)
  (if *test-extensions*
      (dotest (string-append *extensions-enabler* s) expected)))

(define (failure s expected)
  (set! failed-inputs (cons s failed-inputs))
  (set! failed-expected (cons expected failed-expected))
  (display "***** test failed *****")
  (newline)
  (write s)
  (newline)
  (newline)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS error handling.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safely thunk token)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda args
        (k token))
      thunk))))

(define (mustfail name f s)
  (if (call-with-current-continuation
       (lambda (return)
         (with-exception-handler
          (lambda args (return #f))
          (lambda () 
            (f s)
            #t))))
      (begin
        (failure s 'error)
        #f)
      #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Testing the test framework.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest "()" list?)
(dotest "()" '(()))
(dotest ")(" 'error)

(dotest "#!r6rs" '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Sanity and regression tests.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest (teststring #\\ #\x #\3 #\b #\b #\;)
        (list (string->symbol (teststring #x3bb))))

(dotest (teststring #\( #\x3bb #\space #\( #\) #\space #\4 #\2 #\))
        (list (list (string->symbol (teststring #x3bb)) '() 42)))

(dotest (string #\( #\x3bb #\space #\( #\) #\space #\4 #\2 #\) #\linefeed)
        (list (list (string->symbol (teststring #x3bb)) '() 42)))

(dotest "abc;def" '(abc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Beginnings of a more systematic test suite.
;
; <datum> --> <lexeme datum> | <compound datum>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; <lexeme datum> --> <boolean> | <number> | <character>
;                  | <string> | <symbol>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest "#t" '(#t))
(dotest "#T" '(#t))
(dotest "#f" '(#f))
(dotest "#F" '(#f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; <number> --> <num 2> | <num 8> | <num 10> | <num 16>
; <num R> --> <prefix R> <complex R>
; <prefix R> --> <radix R> <exactness> | <exactness> <radix R>
; <exactness> --> <empty> | #i | #I | #e | #E
; <radix 2> --> #b | #B
; <radix 8> --> #o | #O
; <radix 10> --> <empty> | #d | #D
; <radix 16> --> #x | #X
;
; <complex R> --> <real R> | <real R> @ <real R>
;               | <real R> + <ureal R> i | <real R> - <ureal R> i
;               | <real R> + <naninf> i | <real R> - <naninf> i
;               | <real R> + i | <real R> - i
;               | + <ureal R> i | - <ureal R> i
;               | + <naninf> i | - <naninf> i
;               | + i | - i
;
; <real R> --> <sign> <ureal R> | + <naninf> | - <naninf>
; <sign> --> <empty> | + | -
; <naninf> --> nan.0 | inf.0
; <ureal R> --> <uinteger R>
;             | <uinteger R> / <uinteger R>
;             | <decimal R> <mantissa width>
;
; <uinteger R> --> <digit R>+ #*
;
; <decimal 10> --> <uinteger 10>
;                | . <digit 10>+ #* <suffix>
;                | <digit 10>+ . <digit10>* #* <suffix>
;                | <digit 10>+ #* . #* <suffix>
; <suffix> --> <empty> | <exponent marker> <sign> <digit 10>+
; <exponent marker> --> e | E | s | S | f | F | d | D | l | L
; <mantissa width> --> <empty> | <vbar> <digit 10>+
;
; <digit 2> --> 0 | 1
; <digit 8> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
; <digit 10> --> <digit>
; <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
; <digit 16> --> <hex digit>
; <hex digit> --> <digit> | a | b | c | d | e | f | A | B | C | D | E | F
;           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; <character> --> #\<any character>
;               | #\<character name>
;               | #\x<hex scalar value>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest "#\\a#\\b#\\c" '(#\a #\b #\c))

(dotest "#\\a #\\b #\\c" '(#\a #\b #\c))

(dotest (teststring #\# #\\ #x3bb #\space
                    #\# #\\ #x65535 #\space
                    #\# #\\ #x100000)
        '(#\x3bb #\x65535 #\x100000))

(dotest (teststring #\# #\\ #x3bb #\space
                    #\# #\\ #xffff #\space
                    #\# #\\ #x10ffff)
        '(#\x3bb #\xffff #\x10ffff))

(dotest "#\\nul #\\alarm #\\backspace #\\tab #\\linefeed"
        '(#\nul #\alarm #\backspace #\tab #\linefeed))

(dotest "#\\vtab #\\page #\\return #\\esc #\\space #\\delete"
        '(#\vtab #\page #\return #\esc #\space #\delete))

(dotest "#\\x61 #\\x3bb #\\xffff #\\x10ffff"
        '(#\a #\x3bb #\xffff #\x10ffff))

(dotest "#\\newline #\\newline" '(#\linefeed #\linefeed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; <string> --> " <string element>* "
; <string element> --> <any character other than " or \ or line ending>
;                    | <line ending>
;                    | \a | \b | \t | \n | \v | \f | \r
;                    | \" | \\
;                    | \<intraline whitespace>\<line ending>
;                      <intraline whitespace>
;                    | <inline hex escape>
; <intraline whitespace> --> <character tabulation>
;                    | <any character whose category is Zs>
; <line ending> --> <linefeed>
;                 | <carriage return>
;                 | <carriage return> <linefeed>
;                 | <next line>
;                 | <carriage return> <next line>
;                 | <line separator>
; <inline hex escape> --> \x <hex scalar value>;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest "\"abc\"\"def\"" '("abc" "def"))

(dotest (teststring #\" #\linefeed #\return #\return #\linefeed
                    #\x85 #\return #\x85 #\x2028 #\")
        (list (teststring #\linefeed #\linefeed #\linefeed
                          #\linefeed #\linefeed #\linefeed)))

(dotest (teststring #\" #\linefeed #\space #\return #\space
                    #\return #\linefeed #\space
                    #\x85 #\space #\return #\x85 #\space #\x2028 #\")
        (list (teststring #\linefeed #\space #\linefeed #\space
                          #\linefeed #\space #\linefeed #\space
                          #\linefeed #\space #\linefeed)))

(dotest (teststring #\" #\\ #\a #\\ #\b #\\ #\t #\\ #\n
                    #\\ #\v #\\ #\f #\\ #\r #\")
        (list (teststring #\alarm #\backspace #\tab #\linefeed
                          #\vtab #\page #\return)))

(dotest (teststring #\" #\\ #\" #\\ #\\ #\")
        (list (teststring #\" #\\)))

(dotest (teststring #\" #\\ #\linefeed #\\ #\return #\\ #\return #\linefeed
                    #\\ #\x85 #\\ #\return #\x85 #\\ #\x2028 #\")
        (list ""))

(dotest (teststring #\" #\\ #\linefeed #\space #\\ #\return #\space
                    #\\ #\return #\linefeed #\space
                    #\\ #\x85 #\space #\\ #\return #\x85 #\space
                    #\\ #\x2028 #\")
        (list ""))

(dotest "\"\\x61;\\x3bb;\\xffff;\\x10000;\\x10ffff;\""
        (list (teststring #\a #\x3bb #\xffff #\x10000 #\x10ffff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; <symbol> --> <identifier>
; <identifier> --> <initial> <subsequent>*
;                | <peculiar identifier>
; <initial> --> <constituent> | <special initial> | <inline hex escape>
; <constituent> --> <letter>
;                 | <any character whose Unicode scalar value is
;                    greater than 127, and whose category is
;                    Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po,
;                    Sc, Sm, Sk, So, or Co>
; <letter> --> a | ... | z | A | ... | Z
; <special initial> --> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
; <subsequent> --> <initial>
;                | <digit>
;                | <special subsequent>
;                | <any character whose category is Nd, Mc, or Me>
; <special subsequent> --> + | - | . | @
; <peculiar identifier> --> + | - | ... | -> <subsequent>*
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dotest "a A z Z" (map string->symbol '("a" "A" "z" "Z")))

(dotest (teststring #\x3bb #\space #\x10000)
        (map string->symbol (list (string #\x3bb) (string #\x10000))))

(dotest (teststring #\xffff) 'error)

(dotest (teststring #\x10ffff) 'error)

(dotest "! $ % & * / : < = > ? ^ _ ~"
        (map string->symbol
             '("!" "$" "%" "&" "*" "/" ":" "<" "=" ">" "?" "^" "_" "~")))

(dotest "." 'error)

(dotest "@" 'error)

(dotest ".a" 'error)

(dotest "@b" 'error)

(dotest "a|b" 'error)

(dotest "|aBc|" 'error)

(dotest "a\\Bc" 'error)

(dotest "(1 2 3 .c)" 'error)

(dotest "(1 2 3 . c)" '((1 2 3 . c)))

(dotest "\\x3bb; \\x10000;"
        (map string->symbol (list (string #\x3bb) (string #\x10000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Exhaustive tests on very short inputs.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tests all single-character inputs within the specified
; inclusive range of code points.

(define (test-single-characters lo hi)
  (define (loop cp passed failed)
    (if (> cp hi)
        (list passed failed)
        (if (<= #xd800 cp #xdfff)
            (loop (+ cp 1) passed failed)
            (let* ((c (integer->char cp))
                   (s (string c))
                   (cat (char-general-category c))
                   (expected
                    (cond ((< cp 128)
                           (cond ((and (char<=? #\0 c) (char<=? c #\9))
                                  (list (- cp (char->integer #\0))))
                                 ((or (and (char<=? #\a c) (char<=? c #\z))
                                      (and (char<=? #\A c) (char<=? c #\Z))
                                      (memv c '(#\! #\$ #\% #\& #\* #\/ #\:
                                                #\< #\= #\> #\? #\^ #\_ #\~
                                                #\+ #\-)))
                                  (list (string->symbol s)))
                                 ((or (char-whitespace? c)
                                      (char=? c #\;))
                                  '())
                                 (else 'error)))
                          ((memq cat
                                 '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po
                                   Sc Sm Sk So Co))
                           (list (string->symbol s)))
                          ((or (char=? c #\x85)
                               (memq cat '(Zs Zl Zp)))
                           '())
                          (else 'error)))
                   (result (dotest s expected)))
              (if result
                  (loop (+ cp 1) (+ passed 1) failed)
                  (loop (+ cp 1) passed (+ failed 1)))))))
  (loop lo 0 0))

; Tests get-datum against read on all two-character inputs with
; the specified first character and with the second character
; within the specified inclusive range of code points.

(define (test-two-characters c0 lo hi)
  (define (loop cp passed failed)
    (if (> cp hi)
        (list passed failed)
        (if (<= #xd800 cp #xdfff)
            (loop (+ cp 1) passed failed)
            (let* ((c (integer->char cp))
                   (s (string c0 c))
                   (expected
                    (safely (lambda () (read (open-string-input-port s)))
                            'error))
                   (actual
                    (safely (lambda () (get-datum (open-string-input-port s)))
                            'error))
                   (result (equal? expected actual)))
              (if result
                  (loop (+ cp 1) (+ passed 1) failed)
                  (begin (failure s expected)
                         (loop (+ cp 1) passed (+ failed 1))))))))
  (loop lo 0 0))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "read0"))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda ()
       (test-single-characters (hide count input1) (hide count input2))
       (test-two-characters #\a (hide count input1) (hide count input2)))
     (lambda (result) (null? failed-inputs)))))
