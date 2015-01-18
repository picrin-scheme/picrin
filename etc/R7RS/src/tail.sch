;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.
;;;
;;; Modified for R6RS by Will Clinger.
;;;
;;; The key idea of this benchmark is that, for each iteration,
;;; the entire input is read line by line before any output
;;; is produced, and the lines are then written to the output
;;; in the reverse of the order in which they were read.

(import (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (rnrs files))

(define (tail-r-aux port file-so-far)
  (let ((x (get-line port)))
    (if (eof-object? x)
        file-so-far
        (tail-r-aux port (cons x file-so-far)))))

(define (echo-lines-in-reverse-order in out)
  (for-each (lambda (line) (put-string out line) (newline out))
            (tail-r-aux in '())))

(define (go input output)
  (call-with-input-file
   input
   (lambda (in)
     (if (file-exists? output) (delete-file output))
     (call-with-output-file
      output
      (lambda (out)
        (echo-lines-in-reverse-order in out))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 input2)
         (s1 input1)
         (name "tail"))
    (run-r6rs-benchmark
     (string-append name ":" s3)
     count
     (lambda () (go (hide count input1) (hide count input2)))
     (lambda (result) #t))))
