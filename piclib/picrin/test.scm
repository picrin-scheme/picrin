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


    (on-test-begin on-test-begin on-test-begin!)
    (on-test-end on-test-end on-test-end!)
    (on-group-begin on-group-begin on-group-begin!)
    (on-group-end on-group-end on-group-end!)

    (on-test-enter on-test-enter set-on-test-enter!)

    (on-test-pass on-test-pass set-on-test-pass!)
    (on-test-fail on-test-fail set-on-test-fail!)
    (on-test-xpass on-test-xpass set-on-test-xpass!)
    (on-test-xfail on-test-xfail set-on-test-xfail!)
    (on-test-skip on-test-skip set-on-test-skip!)
    (on-test-error on-test-error set-on-test-error!)

    (on-test-exit on-test-exit set-on-test-exit!))

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

  (define (on-test-begin-null r name count) #f)
  (define (on-test-end-null r name) #f)
  (define (on-test-enter-null r) #f)
  (define (on-test-pass-null r name expect form got) #f)
  (define (on-test-fail-null r name expect form got) #f)
  (define (on-test-xpass-null r name expect form got) #f)
  (define (on-test-xfail-null r name expect form got) #f)
  (define (on-test-skip-null r name expect form) #f)
  (define (on-test-error-null r name expect form error) #f)
  (define (on-test-exit-null r) #f)

  (define (test-runner-null)
    (let ((r (make-test-runner)))
      (set-test-count! r 0)
      (set-fail-count! r 0)
      (set-fails! r ())
      (set-xpass-count! r 0)
      (set-xfail-count! r 0)
      (set-skip-count! r 0)
      (set-suit-count! r 0)
      
      (set-current-test! r #f)

      (set-skips! r ())
      (set-xfails! r ())

      (set-on-test-enter! r on-test-enter-null)
      (on-test-begin! r on-test-begin-null)
      (on-test-end! r on-test-end-null)
      (set-on-test-pass! r on-test-pass-null)
      (set-on-test-fail! r on-test-fail-null)
      (set-on-test-xpass! r on-test-xpass-null)
      (set-on-test-xfail! r on-test-xfail-null)
      (set-on-test-skip! r on-test-skip-null)
      (set-on-test-exit! r on-test-exit-null)            
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

  (define (on-test-begin-simple r suit-name count)
    (set-suit-count! r (+ (suit-count r) 1)))

  (define (on-test-end-simple r suit-name)
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


  (define (on-test-enter-simple r)
    (set-test-count! r (+ (test-count r) 1)))

  (define (on-test-pass-simple r test-name expected expr got)
    (print-case (current-output-port) (test-count r) test-name)
    (display "[0;32m")
    (display " PASS: ")
    (write expr)
    (display " equals ")
    (write expected)
    (newline)
    (display "[0;39m"))
  
  (define (on-test-fail-simple r test-name expected expr got)
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

  (define (on-test-xfail-simple r test-name expected expr got)
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

  (define (on-test-xpass-simple r test-name expected expr got)
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


  (define (on-test-skip-simple r test-name expected expr)
    (set-skip-count! r (+ (skip-count r) 1))
    (print-case (test-count r) test-name)
    (display "[0;31m" )
    (display " SKIP: ")
    (write expr)
    (newline)
    (display "[0;39m"))

  (define (on-test-error-simple r test-name expected expr got)
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

  (define (on-test-exit-simple r)
    #f)


  (define (test-runner-simple)
    (let ((r (test-runner-null)))
      (on-test-begin! r on-test-begin-simple)
      (on-test-end! r on-test-end-simple)
      
      (set-on-test-enter! r on-test-enter-simple)
      (set-on-test-pass! r on-test-pass-simple)
      (set-on-test-fail! r on-test-fail-simple)
      (set-on-test-xpass! r on-test-xpass-simple)
      (set-on-test-xfail! r on-test-xfail-simple)
      (set-on-test-skip! r on-test-skip-simple)
      (set-on-test-error! r on-test-error-simple)
      (set-on-test-exit! r on-test-exit-simple)
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
            ((on-test-enter r) r)
            (if (and #;(not (member #t (map (lambda (f) (f r)) (applys r))))
                 (member #t (map (lambda (f) (f r)) (skips r))))
                ((on-test-skip r) r test-name expected 'expr)
                (with-exception-handler
                 (lambda (e)
                   (set-test-error-value! t e)
                   ((on-test-error r) r test-name expected 'expr e))
                 (lambda ()
                   (if (member #t (map (lambda (f) (f r)) (xfails r)))
                       (set-test-xfail! t #t)
                       (set-test-xfail! t #f))
                   (let ((res expr))
                     (set-test-result! t res)
                     (if (test-xfail? t)
                         (if (test? res expected)
                             ((on-test-xpass r) r test-name expected 'expr res)
                             ((on-test-xfail r) r test-name expected 'expr res))
                         (if (test? res expected)
                             ((on-test-pass r) r test-name expected 'expr res)
                             ((on-test-fail r) r test-name expected 'expr res)))))))
            ((on-test-exit r) r)))
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
         ((on-test-begin r) r suit-name count)))
      ((_ suit-name)
       (test-begin suit-name 0))
      ((_)
       (test-begin #f))))


  (define-syntax test-end
    (syntax-rules ()
      ((_ suit-name)
       (let ((r (test-runner-current)))
         (set-skips! r ())
         ((on-test-end r) r suit-name)))
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

  (define test-result-ref
    (case-lambda
     ((runner pname)
      (test-result-ref runner pname #f))
     ((runner pname default)
      (let ((t (current-test (current-test-runner))))
        (and t
             (case pname
               (name (test-name t))
               (expected (test-expected t))
               (form (test-form t))
               (result (test-result t))
               (error-value (test-error-value t))
               (expected-error (test-expected-error t))
               (xfail? (test-xfail? t))
               (else default)))))))

  (export
   test-assert
   test-eqv
   test-equal
   test-eq
   test-approximate

   test-error

                                        ;test-read-eval-string

   test-begin
   test-end
   test-group

   test-group-with-cleanup

   test-match-name
   test-match-nth
   test-match-any
   test-match-all

   test-skip

   test-expect-fail

   test-runner?
   test-runner-current
   test-runner-get
   test-runner-simple
   test-runner-null
   test-runner-create
   test-runner-factory

   test-apply
   test-with-runner

   test-result-kind
                                        ;test-passed?
                                        ;test-result-ref
                                        ;test-result-set!
                                        ;test-result-remove
                                        ;test-result-clear
                                        ;test-result-alist
   (rename on-test-begin test-runner-on-test-begin)
   (rename on-test-begin! test-runner-on-test-begin!)
   (rename on-test-end test-runner-on-test-end)
   (rename on-test-end!  test-runner-on-test-end!)
   (rename on-group-begin test-runner-on-group-begin)
   (rename on-group-begin!  test-runner-on-group-begin!)
   (rename on-group-end test-runner-on-group-end)
   (rename on-group-end! test-runner-on-group-end!)
   ;; test-runner-on-bad-count
   ;; test-runner-on-bad-count!
   ;; test-runner-on-bad-end-name
   ;; test-runner-on-bad-end-name!
   ;; test-runner-on-final
   ;; test-runner-on-final!
   (rename on-test-begin-simple test-on-test-begin-simple)
   (rename on-test-end-simple test-on-test-end-simple)
   ;; test-on-group-begin-simple
   ;; test-on-group-end-simple
   ;; test-on-bad-count-simple
   ;; test-on-bad-end-name-simple

   (rename pass-count test-runner-pass-count)
   (rename fail-count test-runner-fail-count)
   (rename xpass-count test-runner-xpass-count)
   (rename xfail-count test-runner-xfail-count)
   (rename skip-count test-runner-skip-count)
   ;; test-runner-test-name
   ;; test-runner-group-path
   ;; test-runner-group-stack
   ;; test-runner-aux-value
   ;; test-runner-aux-value!
   ;; test-runner-reset
   )

  (export
   test
   test-values
   test-syntax-error)
  )
