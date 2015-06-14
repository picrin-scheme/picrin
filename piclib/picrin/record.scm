(define-library (picrin record)
  (import (picrin base)
          (picrin macro))

  ;; define-record-type

  (define ((boot-make-record-type <meta-type>) name)
    (let ((rectype (make-record <meta-type>)))
      (record-set! rectype 'name name)
      rectype))

  (define <record-type>
    (let ((<record-type>
           ((boot-make-record-type #t) 'record-type)))
      (record-set! <record-type> '@@type <record-type>)
      <record-type>))

  (define make-record-type (boot-make-record-type <record-type>))

  (define-syntax define-record-constructor
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((rectype (car (cdr form)))
	     (name    (car (cdr (cdr form))))
	     (fields  (cdr (cdr (cdr form)))))
	 `(define (,name ,@fields)
	    (let ((record (make-record ,rectype)))
	      ,@(map (lambda (field)
		       `(record-set! record ',field ,field))
		     fields)
	      record))))))

  (define-syntax define-record-predicate
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((rectype (car (cdr form)))
	     (name    (car (cdr (cdr form)))))
	 `(define (,name obj)
	    (and (record? obj)
		 (eq? (record-type obj)
                      ,rectype)))))))

  (define-syntax define-record-field
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((pred       (car (cdr form)))
	     (field-name (car (cdr (cdr form))))
	     (accessor   (car (cdr (cdr (cdr form)))))
	     (modifier?  (cdr (cdr (cdr (cdr form))))))
	 (if (null? modifier?)
	     `(define (,accessor record)
		(if (,pred record)
		    (record-ref record ',field-name)
		    (error (string-append (symbol->string  ',accessor) ": wrong record type") record)))
	     `(begin
		(define (,accessor record)
		  (if (,pred record)
		      (record-ref record ',field-name)
		      (error (string-append (symbol->string  ',accessor) ": wrong record type") record)))
		(define (,(car modifier?) record val)
		  (if (,pred record)
		      (record-set! record ',field-name val)
		      (error (string-append (symbol->string ',(car modifier?)) ": wrong record type")  record)))))))))

  (define-syntax define-record-type
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((name   (car (cdr form)))
	     (ctor   (car (cdr (cdr form))))
	     (pred   (car (cdr (cdr (cdr form)))))
	     (fields (cdr (cdr (cdr (cdr form))))))
	 `(begin
	    (define ,name (make-record-type ',name))
	    (define-record-constructor ,name ,@ctor)
	    (define-record-predicate ,name ,pred)
	    ,@(map (lambda (field) `(define-record-field ,pred ,@field))
		   fields))))))

  (export define-record-type))
