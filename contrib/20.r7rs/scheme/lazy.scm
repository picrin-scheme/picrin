;;; Appendix A. Standard Libraries Lazy

(define-library (scheme lazy)
  (import (scheme base)
	  (picrin macro))

  (define-record-type <promise>
      (make-promise% done value)
      promise?
    (done  promise-done? set-promise-done!)
    (value promise-value set-promise-value!))

  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (make-promise% #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (make-promise% #t expr)))))

  (define (force promise)
    (if (promise-done? promise)
	(promise-value promise)
	(let ((new-promise ((promise-value promise))))
	  (if (promise-done? promise)
	      (promise-value promise)
	      (begin
		(set-promise-done!  promise (promise-done? new-promise))
		(set-promise-value! promise (promise-value new-promise))
		(force promise))))))

  (define (make-promise obj)
    (if (promise? obj)
	obj
	(make-promise% #t obj)))

  (export delay-force
          delay
          force
          make-promise
          promise?))
