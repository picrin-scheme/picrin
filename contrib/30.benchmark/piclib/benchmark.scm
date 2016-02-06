(define-library (picrin benchmark)
  (import (scheme base)
          (scheme time)
          (scheme write))

  (begin

    (define (time f)
      (let ((start (current-jiffy)))
        (f)
        (/ (- (current-jiffy) start)
           (jiffies-per-second))))

    (define-syntax repeat
      (syntax-rules ()
       ((_ n form)
        (let loop ((i n))
          (if (/= i 0)
              (begin
                form
                (loop (- n 1))))))))

    (define-syntax bench
      (syntax-rules ()
        ((_ form)
         (bench 1 form))
        ((_ n form)
         (let ((t (time ((lambda ()) (repeat n form)))))
           (display ""))))))

  (export time
          repeat
          bench))
