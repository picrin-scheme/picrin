(define-library (picrin test)
  (import (picrin base)
          (picrin syntax-rules)
          (picrin record))

  (define-record-type <test-runner>
    (make-test-runner)
    test-runner?
    (test-count test-count set-test-count!)
    (failure-count failure-count set-failure-count!)
    (failures failures set-failures!)
    (suit-count suit-count set-suit-count!))

  (define (test-runner-null)
    (let ((t (make-test-runner)))
      (set-test-count! t 0)
      (set-failure-count! t 0)
      (set-failures! t ())
      (set-suit-count! t 0)
      t))

  (define current-test-runner% (test-runner-null))


  (define (current-test-runner . t)
    (if (null? t)
        current-test-runner%
        (set! current-test-runner% (car t))))

  (define no-name "no test name")

  (define fails '())

  (define (print-statistics)
    (let ((t (current-test-runner)))
      (display "[0;34m")
      (newline)
      (display "Test Result: ")
      (write (- (test-count t) (failure-count t)))
      (display " / ")
      (write (test-count t))
      (display " (")
      (write (* (/ (- (test-count t) (failure-count t)) (test-count t)) 100))
      (display "%)")
      (display " [PASS/TOTAL]")
      (newline)
      (display "[0;39m")
      (for-each
       (lambda (fail)
         (display fail))
       (reverse (failures t)))))

  (define (test-begin . o)
    (let ((t (current-test-runner)))
      (set-suit-count! t (+ (suit-count t) 1))))

  (define (test-end . o)
    (let ((t (current-test-runner)))
      (set-suit-count! t (- (suit-count t) 1))
      (if (= (suit-count t) 0)
          (print-statistics))))

  
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
          (let* ((t (current-test-runner))
                 (call-with-handle-failure (lambda (proc got)
                                             (set-failure-count! t (+ (failure-count t) 1))
                                             (let ((out (open-output-string))
                                                   (str #f))
                                               (print-case out (test-count t) test-name)
                                               (proc out test-name expected 'expr got)
                                               (set! str (get-output-string out))
                                               (set-failures! t (cons str (failures t)))
                                               (display str)))))
            (with-exception-handler
             (lambda (e)
               (call-with-handle-failure print-error e))
             (lambda ()
               (let ((res expr))
                 (cond
                  ((test? res expected)
                   (print-case (current-output-port) (test-count t) test-name)
                   (print-pass (current-output-port) test-name expected 'expr))
                  (else
                   (call-with-handle-failure print-fail res))))))
            (set-test-count! t (+ (test-count t) 1))))
         ((_ expected expr)
          (test no-name expected expr))))))


  (define-syntax test-eq
    (define-test-macro eq?))
  (define-syntax test-eqv
    (define-test-macro eqv?))
  (define-syntax test-equal
    (define-test-macro equal?))
  (define-syntax test
    (define-test-macro equal?))

  (define-syntax test-values
    (syntax-rules ()
      ((_ test-name expect expr)
       (test test-name
             (call-with-values (lambda () expect) (lambda results results))
             (call-with-values (lambda () expr) (lambda results results))))
      ((_ expect expr)
       (test-values no-name expect expr))))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ test-name expr)
       (test-eq test-name #t expr))
      ((_ expr)
       (test-assert no-name))))

  (define-syntax test-approximate
    ;; :TODO: write for better failure message
    (syntax-rules ()
      ((_ test-name expected expr error)
       (test-assert test-name
                    (and (>= expr (- expected error))
                         (<= expr (+ expected error)))))
      ((_ expected expr error)
       (test-approximate no-name expected expr error))))


  (define-syntax test-error
    ;; :TODO: write for better failure message
    (syntax-rules ()
      ((_ test-name error-type expr)
       (with-exception-handler
        (lambda (e)
          (case error-type
            ((error)      (test-assert test-name (error? e)))
            ((file-error) (test-assert test-name (file-error? e)))
            ((read-error) (test-assert test-name (read-error? e)))
            ((#t)         (test-assert test-name (error? e)))
            (else => (lambda (t) (error "test-error: Unknown error type ~a specified." t)))))
        (lambda ()
          expr)))
      ((_ error-type expr)
       (test-error no-name expr))
      ((_ expr)
       (test-error #t expr))))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_) (syntax-error "invalid use of test-syntax-error"))))

  (export test test-begin test-end test-values test-syntax-error))
