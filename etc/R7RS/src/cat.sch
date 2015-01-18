;;; CAT -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic Scheme.

(import (rnrs base)
        (rnrs io simple)
        (rnrs files))

(define (catport in out)
  (let ((x (read-char in)))
    (if (not (eof-object? x))
        (begin
         (write-char x out)
         (catport in out)))))

(define (go input-file output-file)
  (if (file-exists? output-file)
      (delete-file output-file))
  (call-with-input-file
   input-file
   (lambda (in)
     (call-with-output-file
      output-file
      (lambda (out)
        (catport in out))))))
    
(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 input2)
         (s1 input1)
         (name "cat"))
    (run-r6rs-benchmark
     (string-append name ":" s3)
     count
     (lambda () (go (hide count input1) (hide count input2)))
     (lambda (result) #t))))
