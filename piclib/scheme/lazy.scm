;;; Appendix A. Standard Libraries Lazy

(define-library (scheme lazy)
  (import (scheme base)
	  (picrin macro))

  (define-record-type <promise>
    (make-promise% done obj)
    promise?
    (done promise-done? promise-done!)
    (obj promise-value promise-value!))

  (define-syntax delay-force
    (ir-macro-transformer
     (lambda (form rename compare?)
       (let ((expr (cadr form)))
	 `(make-promise% #f (lambda () ,expr))))))

  (define-syntax delay
    (ir-macro-transformer
     (lambda (form rename compare?)
       (let ((expr (cadr form)))
	 `(delay-force (make-promise% #t ,expr))))))

  (define (promise-update! new old)
    (promise-done! old (promise-done? new))
    (promise-value! old (promise-value new)))

  (define (force promise)
    (if (promise-done? promise)
	(promise-value promise)
	(let ((promise* ((promise-value promise))))
	  (unless (promise-done? promise)
		  (promise-update! promise* promise))
	  (force promise))))

  (define (make-promise obj)
    (if (promise? obj)
	obj
	(make-promise% #t obj)))

  (export delay-force delay force make-promise promise?))
