(import (scheme base)
        (scheme write))

(define (print obj)
  (write obj)
  (newline)
  obj)

(print
 (dynamic-wind
     (lambda () (print 'before1))
     (lambda ()
       (define cont #f)
       (print 1)
       (dynamic-wind
	   (lambda () (print 'before2))
	   (lambda ()
	     (print 2)
	     (set! cont (call/cc values)))
	   (lambda () (print 'after2)))
       (dynamic-wind
	   (lambda () (print 'before3))
	   (lambda ()
	     (print 3)
	     (if (procedure? cont)
		 (cont 42)
		 cont))
	   (lambda () (print 'after3))))
     (lambda () (print 'after1))))

; before1
; 1
; before2
; 2
; after2
; before3
; 3
; after3
; before2
; after2
; before3
; 3
; after3
; after1
; => 42

(print
 (let ((path '())
       (c #f))
   (let ((add (lambda (s)
		(set! path (cons s path)))))
     (dynamic-wind
	 (lambda () (add 'connect))
	 (lambda ()
	   (add (call-with-current-continuation
		 (lambda (c0)
		   (set! c c0)
		   'talk1))))
	 (lambda () (add 'disconnect)))
     (if (< (length path) 4)
	 (c 'talk2)
	 (reverse path)))))

; (connect talk1 disconnect connect talk2 disconnect)
