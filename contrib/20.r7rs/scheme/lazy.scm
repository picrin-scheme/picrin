;;; Appendix A. Standard Libraries Lazy

(define-library (scheme lazy)
  (import (scheme base)
	  (picrin macro))

  (define-record-type <promise>
      (make-promise% done value)
      promise?%
    (done  promise-done?% set-promise-done!%)
    (value promise-value% set-promise-value!%))

  (define (box x) (list x))
  (define box? list?)
  (define unbox car)
  (define set-box! set-car!)

  (define (promise? x)
    (promise?% (unbox x)))
  (define (promise-done? x)
    (promise-done?% (unbox x)))
  (define (set-promise-done! boxed x)
    (set-promise-done!% (unbox boxed) x))
  (define (promise-value x)
    (promise-value% (unbox x)))
  (define (set-promise-value! boxed x)
    (set-promise-value!% (unbox boxed) x))

  (define (make-promise%% done value)
    (box (make-promise% done value)))
  
  (define-syntax delay-force
    (syntax-rules ()
      ((_ expr)
       (make-promise%% #f (lambda () expr)))))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (delay-force (make-promise%% #t expr)))))

  (define (force promise)
    (if (promise-done? promise)
	(promise-value promise)
	(let ((new-promise ((promise-value promise))))
	  (set-promise-done!  promise (promise-done? new-promise))
	  (set-promise-value! promise (promise-value new-promise))
	  (set-box! new-promise (unbox promise))
	  (force promise))))

  (define (make-promise obj)
    (if (and (box? obj) (promise? obj))
	obj
	(make-promise%% #t obj)))

  (export delay-force
          delay
          force
          make-promise
          promise?))
