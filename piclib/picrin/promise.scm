(define-library (picrin promise)
  (import (scheme base)
          (picrin experimental lambda))

  (define (identity x)
    x)

  (define-record-type <promise>
      (create-promise status reactors cache)
      promise?
    (status promise-status set-promise-status!)
    (reactors promise-reactors set-promise-reactors!)
    (cache promise-cache set-promise-cache!))

  (define (push-promise-reactor! promise reactor)
    (set-promise-reactors! promise (cons reactor (promise-reactors promise))))

  #;
  (define (print x)
    (write x)
    (newline)
    (flush-output-port)
    x)

  (define (make-promise handler)
    (let ((self (create-promise 'pending '() #f)))

      (define (on-resolved result)
        (when (eq? (promise-status self) 'pending)
          (for-each
           (lambda (((resolve . reject) on-resolved _))
             (call/cc
              (lambda (exit)
                (with-exception-handler
                 (lambda (e)
                   (reject e)
                   (exit #f))
                 (lambda ()
                   (resolve (on-resolved result)))))))
           (promise-reactors self))
          (set-promise-status! self 'resolved)
          (set-promise-cache! self result)
          (set-promise-reactors! self '())))

      (define (on-rejected reason)
        (when (eq? (promise-status 'pending) 'pending)
          (for-each
           (lambda (((resolve . reject) _ on-rejected))
             (call/cc
              (lambda (exit)
                (with-exception-handler
                 (lambda (e)
                   (reject e)
                   (exit #f))
                 (lambda ()
                   (resolve (on-rejected reason)))))))
           (promise-reactors self))
          (set-promise-status! self 'rejected)
          (set-promise-cache! self reason)
          (set-promise-reactors! self '())))

      (handler on-resolved on-rejected)

      self))

  (define (promise-chain self on-resolved on-rejected)

    (define (handler resolve reject)
      (case (promise-status self)
        (pending
         (push-promise-reactor! self `((,resolve . ,reject) ,on-resolved ,on-rejected)))
        (resolved
         (call/cc
          (lambda (exit)
            (with-exception-handler
             (lambda (e)
               (reject e)
               (exit #f))
             (lambda ()
               (resolve (on-resolved (promise-cache self))))))))
        (rejected
         (call/cc
          (lambda (exit)
            (with-exception-handler
             (lambda (e)
               (reject e)
               (exit #f))
             (lambda ()
               (resolve (on-rejected (promise-cache self))))))))))

    (make-promise handler))

  (define (promise-then self on-resolved)
    (promise-chain self on-resolved identity))

  (define (promise-catch self on-rejected)
    (promise-chain self identity on-rejected))

  (define (promise-all promises)

    (define (handler resolve reject)
      (do ((i 0 (+ i 1))
           (x promises (cdr x))
           (c 0)
           (v (make-vector (length promises))))
          ((null? x))

        (define (on-resolved result)
          (vector-set! v i result)
          (set! c (+ c 1))
          (when (= c (length promises))
            (resolve v)))

        (define (on-rejected reason)
          (reject reason))

        (promise-chain (car x) on-resolved on-rejected)))

    (make-promise handler))

  (define (promise-any promises)

    (define (handler resolve reject)
      (do ((i 0 (+ i 1))
           (x promises (cdr x))
           (c 0)
           (v (make-vector (length promises))))
          ((null? x))

        (define (on-resolved result)
          (resolve result))

        (define (on-rejected reason)
          (vector-set! v i reason)
          (set! c (+ c 1))
          (when (= c (length promises))
            (reject v)))

        (promise-chain (car x) on-resolved on-rejected)))

    (make-promise handler))

  ; experimental API
  (define (promise-race promises)
    (make-promise
     (lambda (resolve reject)
       (for-each
        (lambda (x)
          (promise-chain x resolve reject))
        promises))))

  (export promise?
          make-promise
          promise-then
          promise-catch
          promise-all
          promise-any))
