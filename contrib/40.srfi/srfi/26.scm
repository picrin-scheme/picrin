(define-library (srfi 26)
  (import (scheme base)
	  (picrin macro)
	  (srfi 1))

  (define-macro cut%
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((slots (second form))
	     (combi (third form))
	     (se (cdddr form)))
	 (cond ((null? se)
		`(lambda ,slots ((begin ,(car combi)) ,@(cdr combi))))
	       ((and (symbol? (car se))
		     (compare? (car se) '<...>))
		`(lambda (,@slots . rest-slot) (apply ,@combi rest-slot)))
	       ((and (symbol? (car se))
		     (compare? (car se) '<>))
		`(cut% (,@slots x) (,@combi x) ,@(cdr se)))
	       (else `(cut% ,slots (,@combi ,(car se)) ,@(cdr se))))))))

  (define-macro cute%
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((slots (second form))
	     (binds (third form))
	     (combi (fourth form))
	     (se (cddddr form)))
	 (cond ((null? se)
		`(let ,binds
		   (lambda ,slots ((begin ,(car combi)) ,@(cdr combi)))))
	       ((and (symbol? (car se))
		     (compare? (car se) '<...>))
		`(let ,binds
		   (lambda (,@slots . rest-slot) (apply ,@combi rest-slot))))
	       ((and (symbol? (car se))
		     (compare? (car se) '<>))
		`(cute% (,@slots x) ,binds (,@combi x) ,@(cdr se)))
	       (else
		`(cute% ,slots ((x ,(car se)) ,@binds)
			(,@combi x) ,@(cdr se))))))))
  
  (define-macro cut
    (ir-macro-transformer
     (lambda (form inject compare?)
       `(cut% () () ,@(cdr form)))))

  (define-macro cute
    (ir-macro-transformer
     (lambda (form inject compare?)
       `(cute% () () () ,@(cdr form)))))

  (export cut cute))
