(define-library (picrin test)
  (import (picrin base)
          (picrin syntax-rules)
          (picrin record))

  (define-record-type <test-runner>
    (make-test-runner)
    test-runner?

    (test-count test-count set-test-count!)
    (fail-count fail-count set-fail-count!)
    (xpass-count xpass-count set-xpass-count!)
    (xfail-count xfail-count set-xfail-count!)
    (skip-count skip-count set-skip-count!)
    (suit-count suit-count set-suit-count!)

    (current-test current-test set-current-test!)

    (fails fails set-fails!)
    (skips skips set-skips!)
    (applys applys set-applys!)
    (xfails xfails set-xfails!)


    (cb-begin cb-begin set-cb-begin!)
    (cb-end cb-end set-cb-end!)
    (cb-test-enter cb-test-enter set-cb-test-enter!)
    (cb-test-pass cb-test-pass set-cb-test-pass!)
    (cb-test-fail cb-test-fail set-cb-test-fail!)
    (cb-test-xpass cb-test-xpass set-cb-test-xpass!)
    (cb-test-xfail cb-test-xfail set-cb-test-xfail!)
    (cb-test-skip cb-test-skip set-cb-test-skip!)
    (cb-test-error cb-test-error set-cb-test-error!)
    (cb-test-exit cb-test-exit set-cb-test-exit!))

  (define-record-type <test>
    (make-test name expected form)
    test?
    (test-name test-name set-test-name!)
    (test-expected test-expected set-test-expected!)
    (test-form test-form set-test-form!)
    (test-result test-result set-test-result!)
    (test-result-kind test-result-kind)
    (test-error-value test-error-value set-test-error-value!)
    (test-expected-error test-expected-erro set-test-expected-error!)
    (test-xfail? test-xfail? set-test-xfail!))

  (define (null-cb-begin r name count) #f)
  (define (null-cb-end r name) #f)
  (define (null-cb-test-enter r) #f)
  (define (null-cb-test-pass r name expect form got) #f)
  (define (null-cb-test-fail r name expect form got) #f)
  (define (null-cb-test-xpass r name expect form got) #f)
  (define (null-cb-test-xfail r name expect form got) #f)
  (define (null-cb-test-skip r name expect form) #f)
  (define (null-cb-test-error r name expect form error) #f)
  (define (null-cb-test-exit r) #f)

  (define (test-runner-null)
    (let ((r (make-test-runner)))
      (set-test-count! r 0)
      (set-fail-count! r 0)
      (set-fails! r ())
      (set-xpass-count! r 0)
      (set-xfail-count! r 0)
      (set-skip-count! r 0)
      (set-suit-count! r 0)
      
      (set-skips! r ())
      (set-xfails! r ())

      (set-cb-test-enter! r null-cb-test-enter)
      (set-cb-begin! r null-cb-begin)
      (set-cb-end! r null-cb-end)
      (set-cb-test-pass! r null-cb-test-pass)
      (set-cb-test-fail! r null-cb-test-fail)
      (set-cb-test-xpass! r null-cb-test-xpass)
      (set-cb-test-xfail! r null-cb-test-xfail)
      (set-cb-test-skip! r null-cb-test-skip)
      (set-cb-test-exit! r null-cb-test-exit)            
      r))

  (define current-test-runner #f)
  (define current-test-factory #f)

  (define (test-runner-current . r)
    (if (null? r)
        current-test-runner
        (set! current-test-runner (car r))))
  (define test-runner-get test-runner-current)
  (define (test-runner-factory . f)
    (if (null? f)
        current-test-factory
        (set! current-test-factory (car f))))
  (define (test-runner-create)
    ((test-runner-factory)))

  (define no-name "no test name")

  (define (print-statistics r)
    (display "[0;34m")
    (newline)
    (display "Test Result: ")
    (write (- (test-count r) (fail-count r)))
    (display " / ")
    (write (test-count r))
    (display " (")
    (write (* (/ (- (test-count r) (fail-count r)) (test-count r)) 100))
    (display "%)")
    (display " [PASS/TOTAL]")
    (newline)
    (display "[0;39m")
    (for-each
     (lambda (fail)
       (display fail))
     (reverse (fails r))))


  (define (print-case out number test-name)
    (display "case " out)
    (write number out)
    (display ": " out)
    (display test-name out)
    (newline out))

  (define (simple-cb-begin r suit-name count)
    (set-suit-count! r (+ (suit-count r) 1)))

  (define (simple-cb-end r suit-name)
    (set-suit-count! r (- (suit-count r) 1))
    (if (= (suit-count r) 0)
        (print-statistics r)))

  (define (call-with-handle-fail r test-name expected expr got proc)
    (set-fail-count! r (+ (fail-count r) 1))
    (let ((out (open-output-string))
          (str #f))
      (print-case out (test-count r) test-name)
      (proc out)
      (set! str (get-output-string out))
      (set-fails! r (cons str (fails r)))
      (display str)))


  (define (simple-cb-test-pass r test-name expected expr got)
    (print-case (current-output-port) (test-count r) test-name)
    (display "[0;32m")
    (display " PASS: ")
    (write expr)
    (display " equals ")
    (write expected)
    (newline)
    (display "[0;39m"))
  
  (define (simple-cb-test-fail r test-name expected expr got)
    (call-with-handle-fail r test-name expected expr got
                           (lambda (out) 
                             (display "[0;31m" out)
                             (display " FAIL: " out)
                             (write expr out)
                             (newline out)
                             (display " # expected " out)
                             (write expected out)
                             (display " but got " out)
                             (write got out)
                             (newline out)
                             (display "[0;39m" out))))

  (define (simple-cb-test-xfail r test-name expected expr got)
    (set-xfail-count! r (+ (xfail-count r) 1))
    (print-case (test-count r) test-name)
    (display "[0;31m")
    (display " XFAIL: ")
    (write expr)
    (newline)
    (display " # expected not ")
    (write expected)
    (display " and got ")
    (write got)
    (newline)
    (display "[0;39m"))

  (define (simple-cb-test-xpass r test-name expected expr got)
    (set-xpass-count! r (+ (xpass-count r) 1))
    (print-case (test-count r) test-name)
    (display "[0;31m")
    (display " XPASS: ")
    (write expr)
    (newline)
    (display " # expected not ")
    (write expected)
    (display " but got ")
    (write got)
    (newline)
    (display "[0;39m"))


  (define (simple-cb-test-skip r test-name expected expr)
    (set-skip-count! r (+ (skip-count r) 1))
    (print-case (test-count r) test-name)
    (display "[0;31m" )
    (display " SKIP: ")
    (write expr)
    (newline)
    (display "[0;39m"))

  (define (simple-cb-test-error r test-name expected expr got)
    (call-with-handle-fail r test-name expected expr got
                           (lambda (out)
                             (display "[0;31m" out)
                             (display " FAIL: " out)
                             (write expr out)
                             (newline out)
                             (display " # expected " out)
                             (write expected out)
                             (display " but raised " out)
                             (write got out)
                             (newline out)
                             (display "[0;39m" out))))

  (define (simple-cb-test-exit r)
    (set-test-count! r (+ (test-count r) 1)))


  (define (test-runner-simple)
    (let ((r (test-runner-null)))
      (set-cb-begin! r simple-cb-begin)
      (set-cb-end! r simple-cb-end)
      
      (set-cb-test-pass! r simple-cb-test-pass)
      (set-cb-test-fail! r simple-cb-test-fail)
      (set-cb-test-xpass! r simple-cb-test-xpass)
      (set-cb-test-xfail! r simple-cb-test-xfail)
      (set-cb-test-skip! r simple-cb-test-skip)
      (set-cb-test-error! r simple-cb-test-error)
      (set-cb-test-exit! r simple-cb-test-exit)
      r))

  (define (test-match-name name)
    (lambda (r)
      (equal? name (test-name (current-test r)))))

  (define (test-match-nth n . count)
    (let ((counter 1))
      (lambda (r)
        (if (<= n counter (+ n count))
            #t
            #f))))

  (define (canonicalize-specifier s)
    (cond
     ((number? s)
      (test-match-nth 1 s))
     ((string? s)
      (test-match-name s))
     ((procedure? s)
      s)
     (else (error "error: specifier is expected a number, a string or a procedure but got other."))))

  (define (test-match-any . rest)
    (let ((specifiers (map canonicalize-specifier rest)))
      (lambda (r)
        (member #t (map (lambda (f) (f r)) specifiers)))))
  (define (test-match-all . rest)
    (let ((specifiers (map canonicalize-specifier rest)))
      (lambda (r)
        (not (member #t (map (lambda (f) (not (f r))) specifiers))))))

  (define (test-skip specifier)
    (let ((r (test-runner-current))
          (s (canonicalize-specifier specifier)))
      (set-skips! r (cons s r))))

  (define (test-expect-fail specifier)
    (let ((r (test-runner-current))
          (s (canonicalize-specifier specifier)))
      (set-xfails! r (cons s r))))

  (define-syntax define-test-macro
    (syntax-rules ()
      ((_ test?)
       (syntax-rules ()
         ((_ test-name expected expr)
          (let* ((r (test-runner-current))
                 (t (make-test test-name expected expr)))
            (set-current-test! r t)
            ((cb-test-enter r) r)
            (if (and (not (member #t (map (lambda (f) (f r)) (applys r))))
                     (member #t (map (lambda (f) (f r)) (skips r))))
                ((cb-test-skip r) r test-name expected 'expr)
                (with-exception-handler
                 (lambda (e)
                   (set-test-error-value! t e)
                   ((cb-test-error r) r test-name expected 'expr e))
                 (lambda ()
                   (if (member #t (map (lambda (f) (f r)) (xfails r)))
                       (set-test-xfail! t #t)
                       (set-test-xfail! t #f))
                   (let ((res expr))
                     (set-test-result! t res)
                     (if (test-xfail? t)
                         (if (test? res expected)
                             ((cb-test-xpass r) r test-name expected 'expr res)
                             ((cb-test-xfail r) r test-name expected 'expr res))
                         (if (test? res expected)
                             ((cb-test-pass r) r test-name expected 'expr res)
                             ((cb-test-fail r) r test-name expected 'expr res)))))))
            ((cb-test-exit r) r)))
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
            (else => (lambda (type) (error (string-append "test-error: Unknown error type " (symbol->string type) " specified."))))))
        (lambda ()
          expr)))
      ((_ error-type expr)
       (test-error no-name expr))
      ((_ expr)
       (test-error #t expr))))

  (define-syntax test-syntax-error
    (syntax-rules ()
      ((_) (syntax-error "invalid use of test-syntax-error"))))


  (define-syntax test-begin
    (syntax-rules ()
      ((_ suit-name count)
       (let ((r (test-runner-current)))         
         ((cb-begin r) r suit-name count)))
      ((_ suit-name)
       (test-begin suit-name 0))
      ((_)
       (test-begin #f))))


  (define-syntax test-end
    (syntax-rules ()
      ((_ suit-name)
       (let ((r (test-runner-current)))
         (set-skips! r ())
         ((cb-end r) r suit-name)))
      ((_)
       (test-end #f))))

  (define-syntax test-group
    (syntax-rules ()
      ((_ name decl-or-expr ...)
       (dynamic-wind
           (lambda () (test-begin name))
           (lambda () decl-or-expr ...)
           (lambda () (test-end name))))))

  (define-syntax test-group-with-cleanup
    (syntax-rules ()
      ((_ name decl-or-expr ... cleanup-form)
       (dynamic-wind
           (lambda () (test-begin name))
           (lambda () decl-or-expr ...)
           (lambda () (test-end name)
                   cleanup-form)))))

  (define-syntax test-with-runner
    (syntax-rules ()
      ((_ r decl-or-expr ...)
       (let ((prev-runner (test-runner-current)))
         (dynamic-wind
             (lambda ()
               (test-runner-current r)
               (test-begin))
             (lambda () decl-or-expr ...)
             (lambda ()
               (test-end)
               (test-runner-current prev-runner)))))))

  (define (test-apply r-or-s s-or-p . rest)
    (let ((runner #f)
          (specifiers #f)
          (proc #f))
      (cond
       ((test-runner? r-or-s)
        (set! runner r-or-s)
        (set! specifiers (list s-or-p)))
       ((null? rest)
        (set! runner (test-runner-current))
        (set! specifiers r-or-s)
        (set! proc s-or-p))
       (else
        (set! runner (test-runner-current))
        (set! specifiers (list r-or-s s-or-p))))
      (if (not proc)
          (begin
            (set! rest (reverse rest))
            (set! proc (car rest))
            (set! specifiers (map canonicalize-specifier (append specifiers (cdr rest))))))
      (let ((prev-runner (test-runner-current))
            (prev-applys (applys runner)))
        (dynamic-wind
            (lambda ()
              (test-runner-current runner)
              (set-applys! runner (append specifiers prev-applys))
              (test-begin))
            proc
            (lambda ()
              (test-end)
              (set-applys! runner prev-applys)
              (test-runner-current prev-runner))))
      ))

  (test-runner-factory test-runner-simple)
  (test-runner-current (test-runner-simple))

  (export test test-begin test-end test-values test-syntax-error))
