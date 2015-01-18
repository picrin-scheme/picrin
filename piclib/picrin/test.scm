(define-library (picrin test)
  (import (picrin base)
          (picrin syntax-rules))

  (define test-counter 0)
  (define counter 0)
  (define failure-counter 0)

  (define no-name "no test name")

  (define fails '())

  (define (print-statistics)
    (display "[0;34m")
    (newline)
    (display "Test Result: ")
    (write (- counter failure-counter))
    (display " / ")
    (write counter)
    (display " (")
    (write (* (/ (- counter failure-counter) counter) 100))
    (display "%)")
    (display " [PASS/TOTAL]")
    (newline)
    (display "[0;39m")
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

  
  (define (print-case out number test-name)
    (display "case " out)
    (write number out)
    (display ": " out)
    (display test-name out)
    (newline out))

  (define (print-pass out test-name expected expr)
    (display "[0;32m" out)
    (display " PASS: " out)
    (write expr out)
    (display " equals " out)
    (write expected out)
    (newline out)
    (display "[0;39m" out))

  (define (print-fail out test-name expected expr res) 
    (display "[0;31m" out)
    (display " FAIL: " out)
    (write expr out)
    (newline out)
    (display " # expected " out)
    (write expected out)
    (display " but got " out)
    (write res out)
    (newline out)
    (display "[0;39m" out))

  (define (print-error out test-name expected expr res)
    (display "[0;31m" out)
    (display " FAIL: " out)
    (write expr out)
    (newline out)
    (display " # expected " out)
    (write expected out)
    (display " but raised " out)
    (write res out)
    (newline out)
    (display "[0;39m" out))

  (define-syntax define-test-macro
    (syntax-rules ()
      ((_ test?) 
       (syntax-rules ()
         ((_ test-name expected expr)
          (begin
            (let
                ((call-with-handle-failure (lambda (proc got)
                                             (set! failure-counter (+ failure-counter 1))
                                             (let ((out (open-output-string))
                                                   (str #f))
                                               (print-case out counter test-name)
                                               (proc out test-name expected 'expr got)
                                               (set! str (get-output-string out))
                                               (set! fails (cons str fails))
                                               (display str)))))
              (with-exception-handler
               (lambda (e)
                 (call-with-handle-failure print-error e))
               (lambda ()
                 (let ((res expr))
                   (cond
                    ((test? res expected)
                     (print-case (current-output-port) counter test-name)
                     (print-pass (current-output-port) test-name expected 'expr))
                    (else
                     (call-with-handle-failure print-fail res)))))))
            (set! counter (+ counter 1))))
         ((_ expected expr)
          (test no-name expected expr))))))



  (define-syntax test
    (define-test-macro equal?))

  (define-syntax test-values
    (syntax-rules ()
      ((_ test-name expect expr)
       (test test-name
             (call-with-values (lambda () expect) (lambda results results))
             (call-with-values (lambda () expr) (lambda results results))))
      ((_ expect expr)
       (test (call-with-values (lambda () expect) (lambda results results))
             (call-with-values (lambda () expr) (lambda results results))))))


  (define (test-failure-count)
    (length fails))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_) (syntax-error "invalid use of test-syntax-error"))))

  (export test test-begin test-end test-values test-syntax-error))
