(define-library (picrin dictionary)
  (import (picrin base))

  (define (plist->dictionary plist)
    (let ((dict (make-dictionary)))
      (do ((kv plist (cddr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cadr kv)))))

  (define (alist->dictionary alist)
    (let ((dict (make-dictionary)))
      (do ((kv alist (cdr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cdr kv)))))

  (define (dictionary . plist)
    (plist->dictionary plist))

  (export dictionary?
          dictionary
          make-dictionary
          dictionary-ref
          dictionary-set!
          dictionary-delete
          dictionary-size
          plist->dictionary
          alist->dictionary))
