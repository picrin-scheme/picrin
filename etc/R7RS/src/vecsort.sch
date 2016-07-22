;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Vector sorting benchmark.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (rnrs base)
        (rnrs sorting)
        (rnrs control)
        (rnrs io simple)
        (rnrs arithmetic fixnums))

; Returns a vector of all Unicode characters from lo to hi,
; inclusive.

(define (all-characters lo hi)
  (define (loop sv0 sv1 chars)
    (cond ((fx<? sv1 sv0)
           chars)
          ((or (fx<? sv1 #xd800)
               (fx<? #xdfff sv1))
           (loop sv0 (fx- sv1 1) (cons (integer->char sv1) chars)))
          (else
           (loop sv0 #xd7ff chars))))
  (list->vector (loop (char->integer lo) (char->integer hi) '())))

(define (hash<? c0 c1)
  (define (hash c)
    (let ((sv (char->integer c)))
      (fx- sv (fxxor sv (fxarithmetic-shift-right sv 2)))))
  (fx<? (hash c0) (hash c1)))

(define (hash-then-sort chars)
  (vector-sort char<?
               (vector-sort hash<? chars)))        

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "vecsort")
         (chars
          (hide count
                (all-characters
                 (integer->char input1) (integer->char input2)))))
    (run-r6rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (hash-then-sort chars))
     (lambda (result) (equal? result chars)))))
