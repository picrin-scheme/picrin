;;; CAT -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic Scheme
;;;     and to use UTF-8 transcoding.

(import (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (rnrs files))

(define (catport in out)
  (let ((x (get-char in)))
    (if (not (eof-object? x))
        (begin
         (put-char out x)
         (catport in out)))))

(define (go input-file output-file)
  (let ((t (make-transcoder (utf-8-codec))))
    (if (file-exists? output-file)
        (delete-file output-file))
    (call-with-port
     (open-file-input-port input-file (file-options) 'block t)
     (lambda (in)
       (call-with-port
        (open-file-output-port output-file (file-options) 'block t)
        (lambda (out)
          (catport in out)))))))
    
(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 input2)
         (s1 input1)
         (name "cat:utf-8"))
    (run-r6rs-benchmark
     (string-append name ":" s3)
     count
     (lambda () (go (hide count input1) (hide count input2)))
     (lambda (result) #t))))
