(define-library (picrin test)
  (import (scheme base)
          (scheme write))

  (define test-counter 0)
  (define counter 0)
  (define failure-counter 0)

  (define fails '())

  (define (print-statistics)
    (newline)
    (display "[0;34mTest Result: ")
    (write (- counter failure-counter))
    (display " / ")
    (write counter)
    (display " (")
    (write (* (/ (- counter failure-counter) counter) 100))
    (display "%)")
    (display " [PASS/TOTAL]")
    (display "[0;39m")
    (newline)
    (for-each
     (lambda (fail)
       (display fail))
     (reverse fails)))

  (define (test-begin . o)
    (set! test-counter (+ test-counter 1)))

  (define (test-end . o)
    (set! test-counter (- test-counter 1))
    (if (= test-counter 0)
        (print-statistics)))

  (define-syntax test
    (syntax-rules ()
      ((test expected expr)
       (test expected expr equal?))
      ((test expected expr =)
       (let ((res expr))
         (display "case ")
         (write counter)
         (if (= res expected)
           (begin
             (display "[0;32m PASS: ")
             (write 'expr)
             (display " equals ")
             (write expected)
             (display "[0;39m")
             (newline))
           (begin
             (set! failure-counter (+ failure-counter 1))
             (let ((out (open-output-string)))
               (display " [0;31mFAIL: " out)
               (write 'expr out)
               (newline out)
               (display "   expected " out)
               (write expected out)
               (display " but got " out)
               (write res out)
               (display "[0;39m" out)
               (newline out)
               (let ((str (get-output-string out)))
                 (set! fails (cons str fails))
                 (display str)))))
         (set! counter (+ counter 1))))))

  (define-syntax test-values
    (syntax-rules ()
      ((_ expect expr)
       (test (call-with-values (lambda () expect) (lambda results results))
             (call-with-values (lambda () expr) (lambda results results))))))


  (define (test-failure-count)
    (length fails))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_) (syntax-error "invalid use of test-syntax-error"))))

  (export test test-begin test-end test-values test-syntax-error))
