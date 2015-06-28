(define-library (picrin class)
  (import (scheme base))

  (define-record-type class-type
      (make-class membership)
      class?
    (membership class-membership))

  (define-syntax define-class
    (syntax-rules ()
      ((define-class name membership)
       (define name (make-class membership)))))

  (define (instance? obj class)
    ((class-membership class) obj))

  (define-class <class> class?)

  (define-class <any> (lambda (x) #t))
  (define-class <list> list?)
  (define-class <procedure> procedure?)
  (define-class <number> number?)
  (define-class <boolean> boolean?)
  (define-class <string> string?)

  (export make-class
          instance?
          define-class
          <class>
          <any>
          <list>
          <procedure>
          <number>
          <boolean>
          <string>))
