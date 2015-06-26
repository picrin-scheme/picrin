(define-library (picrin record)
  (import (picrin base)
          (picrin macro))

  ;; record meta type

  (define ((boot-make-record-type <meta-type>) name)
    (let ((rectype (make-record <meta-type>)))
      (record-set! rectype 'name name)
      rectype))

  (define <record-type>
    (let ((<record-type> ((boot-make-record-type #t) 'record-type)))
      (record-set! <record-type> '@@type <record-type>)
      <record-type>))

  (define make-record-type (boot-make-record-type <record-type>))

  ;; define-record-type

  (define-syntax (define-record-constructor type name . fields)
    (let ((record #'record))
      #`(define (#,name . #,fields)
          (let ((#,record (make-record #,type)))
            #,@(map (lambda (field) #`(record-set! #,record '#,field #,field)) fields)
            #,record))))

  (define-syntax (define-record-predicate type name)
    #`(define (#,name obj)
        (and (record? obj)
             (eq? (record-type obj) #,type))))

  (define-syntax (define-record-accessor pred field accessor)
    #`(define (#,accessor record)
        (if (#,pred record)
            (record-ref record '#,field)
            (error (string-append (symbol->string  '#,accessor) ": wrong record type") record))))

  (define-syntax (define-record-modifier pred field modifier)
    #`(define (#,modifier record val)
        (if (#,pred record)
            (record-set! record '#,field val)
            (error (string-append (symbol->string '#,modifier) ": wrong record type")  record))))

  (define-syntax (define-record-field pred field accessor . modifier-opt)
    (if (null? modifier-opt)
        #`(define-record-accessor #,pred #,field #,accessor)
        #`(begin
            (define-record-accessor #,pred #,field #,accessor)
            (define-record-modifier #,pred #,field #,(car modifier-opt)))))

  (define-syntax (define-record-type name ctor pred . fields)
    #`(begin
        (define #,name (make-record-type '#,name))
        (define-record-constructor #,name #,@ctor)
        (define-record-predicate #,name #,pred)
        #,@(map (lambda (field) #`(define-record-field #,pred #,@field)) fields)))

  (export define-record-type))
