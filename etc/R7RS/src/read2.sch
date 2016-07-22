;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test of R6RS get-datum, comparable to the parsing benchmark.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (rnrs base)
        (rnrs control)
        (rnrs io ports)
        (rnrs io simple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; get-datum benchmark comparable to the parsing benchmark.
;
; Reads nboyer.sch into a string before timing begins.
;
; The timed portion of the benchmark parses the string
; representation of nboyer.sch 1000 times.
;
; The output of that parse is checked by comparing it
; the the value returned by the read procedure.
;
; Usage:
;     (read-benchmark n input)
;     (read-from-string-port-benchmark n input)
;     
;
; n defaults to 1000, and input defaults to "nboyer.sch".
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-from-file-benchmark input t)
  (call-with-port
   (open-file-input-port input (file-options) 'block t)
   (lambda (in)
     (do ((x (read in) (read in))
          (y #f x)
          (i 0 (+ i 1)))
         ((eof-object? x) y)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "read1:utf-8")
         (t (make-transcoder (utf-8-codec))))
    (run-r6rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (read-from-file-benchmark (hide count input1) t))
     (lambda (result) (equal? result output)))))
