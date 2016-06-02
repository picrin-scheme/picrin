;;; Appendix A. Standard Libraries Lazy

(define-library (scheme lazy)
  (import (scheme base))

  ;; type 'a <promise> = cached of 'a | chained of 'a promise | pending of () -> 'a promise

  (define-record-type <promise>
      (promise state value)
      promise?
    (state promise-state set-promise-state!)
    (value promise-value set-promise-value!))

  (define (make-promise obj)
    (if (promise? obj)
	obj
	(promise 'cached obj)))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (promise (string->symbol "pending") (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (make-promise expr)))))

  (define (force p)
    (let ((v (promise-value p)))
      (case (promise-state p)
        ((cached)  v)
        ((chained) (let ()
                     (when (eq? 'cached (promise-state v))
                       (set-promise-state! p 'cached)
                       (set-promise-value! p (promise-value v)))
                     (force v)))
        ((pending) (let ((q (v)))
                     (when (eq? 'pending (promise-state p))
                       (set-promise-state! p 'chained)
                       (set-promise-value! p q))
                     (force p))))))

  (export delay-force
          delay
          force
          make-promise
          promise?))
