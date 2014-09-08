(define-library (picrin record)
  (import (picrin base)
          (scheme base))

  ;; define-record-writer

  (define (set-record-writer! record-type writer)
    (record-set! record-type 'writer writer))

  (define-syntax define-record-writer
    (syntax-rules ()
      ((_ (type obj) body ...)
       (set-record-writer! type
         (lambda (obj)
           body ...)))
      ((_ type writer)
       (set-record-writer! type
         writer))))

  ;; define-record-type

  (define ((default-record-writer ctor) obj)
    (let ((port (open-output-string)))
      (display "#.(" port)
      (display (car ctor) port)
      (for-each
       (lambda (field)
         (display " " port)
         (write (record-ref obj field) port))
       (cdr ctor))
      (display ")" port)
      (get-output-string port)))

  (define ((boot-make-record-type <meta-type>) name ctor)
    (let ((rectype (make-record <meta-type>)))
      (record-set! rectype 'name name)
      (record-set! rectype 'writer (default-record-writer ctor))
      rectype))

  (define <record-type>
    (let ((<record-type>
           ((boot-make-record-type #t) 'record-type '(record-type name writer))))
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
		    (error "wrong record type")))
	     `(begin
		(define (,accessor record)
		  (if (,pred record)
		      (record-ref record ',field-name)
		      (error "wrong record type")))
		(define (,(car modifier?) record val)
		  (if (,pred record)
		      (record-set! record ',field-name val)
		      (error "wrong record type")))))))))

  (define-syntax define-record-type
    (ir-macro-transformer
     (lambda (form inject compare?)
       (let ((name   (car (cdr form)))
	     (ctor   (car (cdr (cdr form))))
	     (pred   (car (cdr (cdr (cdr form)))))
	     (fields (cdr (cdr (cdr (cdr form))))))
	 `(begin
	    (define ,name (make-record-type ',name ',ctor))
	    (define-record-constructor ,name ,@ctor)
	    (define-record-predicate ,name ,pred)
	    ,@(map (lambda (field) `(define-record-field ,pred ,@field))
		   fields))))))

  (export define-record-type
          define-record-writer))
