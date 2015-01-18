(define-library (picrin test)
  (import (picrin base)
          (picrin syntax-rules)
          (picrin record))

  (define-record-type <test-runner>
    (make-test-runner)
    test-runner?
    ;; members
    (test-count test-count set-test-count!)
    (failure-count failure-count set-failure-count!)
    (failures failures set-failures!)
    (suit-count suit-count set-suit-count!)

    ;; call backs
    (cb-begin cb-begin set-cb-begin!)
    (cb-end cb-end set-cb-end!)
    (cb-test-enter cb-test-enter set-cb-test-enter!)
    (cb-test-pass cb-test-pass set-cb-test-pass!)
    (cb-test-failure cb-test-failure set-cb-test-failure!)
    (cb-test-error cb-test-error set-cb-test-error!)
    (cb-test-exit cb-test-exit set-cb-test-exit!))

  (define (null-cb-begin t name count) #f)
  (define (null-cb-end t name) #f)
  (define (null-cb-test-enter t) #f)
  (define (null-cb-test-pass    t name expect form got) #f)
  (define (null-cb-test-failure t name expect form got) #f)
  (define (null-cb-test-error   t name expect form error) #f)
  (define (null-cb-test-exit t) #f)

  (define (test-runner-null)
    (let ((t (make-test-runner)))
      (set-test-count! t 0)
      (set-failure-count! t 0)
      (set-failures! t ())
      (set-suit-count! t 0)

      (set-cb-test-enter! t null-cb-test-enter)
      (set-cb-begin! t null-cb-begin)
      (set-cb-end! t null-cb-end)
      (set-cb-test-pass! t null-cb-test-pass)
      (set-cb-test-exit! t null-cb-test-exit)
      t))

  (define current-test-runner% #f)
  (define (current-test-runner . t)
    (if (null? t)
        current-test-runner%
        (set! current-test-runner% (car t))))

  (define no-name "no test name")

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

  (define (simple-cb-begin t suit-name count)
    (set-suit-count! t (+ (suit-count t) 1)))

  (define (simple-cb-end t suit-name)
    (set-suit-count! t (- (suit-count t) 1))
    (if (= (suit-count t) 0)
        (print-statistics)))

  (define (call-with-handle-failure t test-name expected expr got proc)
    (set-failure-count! t (+ (failure-count t) 1))
    (let ((out (open-output-string))
          (str #f))
      (print-case out (test-count t) test-name)
      (proc out test-name expected 'expr got)
      (set! str (get-output-string out))
      (set-failures! t (cons str (failures t)))
      (display str)))


  (define (simple-cb-test-pass t test-name expected expr got)
    (print-case (current-output-port) (test-count t) test-name)
    (print-pass (current-output-port) test-name expected expr))
  
  (define (simple-cb-test-failure t test-name expected expr got)
    (call-with-handle-failure t test-name expected expr got
                              print-fail))
  (define (simple-cb-test-error t test-name expected expr got)
    (call-with-handle-failure t test-name expected expr got
                              print-error))
  (define (simple-cb-test-exit t)
    (set-test-count! t (+ (test-count t) 1)))


  (define (test-runner-simple)
    (let ((t (test-runner-null)))
      (set-cb-begin! t simple-cb-begin)
      (set-cb-end! t simple-cb-end)
      
      (set-cb-test-pass! t simple-cb-test-pass)
      (set-cb-test-failure! t simple-cb-test-failure)
      (set-cb-test-error! t simple-cb-test-error)
      (set-cb-test-exit! t simple-cb-test-exit)
      t))


  (define-syntax test-begin
    (syntax-rules ()
      ((_ suit-name count)
       (let ((t (current-test-runner)))
         ((cb-begin t) t suit-name count)))
      ((_ suit-name)
       (test-begin suit-name 0))
      ((_)
       (test-begin #f))))


  (define-syntax test-end
    (syntax-rules ()
      ((_ suit-name)
       (let ((t (current-test-runner)))
         ((cb-end t) t suit-name)))
      ((_)
       (test-end #f))))

  (define-syntax define-test-macro
    (syntax-rules ()
      ((_ test?) 
       (syntax-rules ()
         ((_ test-name expected expr)
          (let* ((t (current-test-runner)))
            ((cb-test-enter t) t)
            (with-exception-handler
             (lambda (e)
               ((cb-test-error t) t test-name expected 'expr e))
             (lambda ()
               (let ((res expr))
                 (cond
                  ((test? res expected)
                   ((cb-test-pass t) t test-name expected 'expr res))
                  (else
                   ((cb-test-failure t) t test-name expected 'expr res))))))
            ((cb-test-exit t) t)))
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

  (current-test-runner (test-runner-simple))

  (export test test-begin test-end test-values test-syntax-error))
