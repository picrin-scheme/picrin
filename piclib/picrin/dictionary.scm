(define-library (picrin dictionary)
  (import (scheme base))

  (define (dictionary->plist dict)
    (error "not implemented"))

  (define (plist->dictionary plist)
    (let ((dict (make-dictionary)))
      (do ((kv plist (cddr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cadr kv)))))

  (define (dictionary->alist dict)
    (error "not implemented"))

  (define (alist->dictionary alist)
    (let ((dict (make-dictionary)))
      (do ((kv alist (cdr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cdr kv)))))

  (define (dictionary . plist)
    (plist->dictionary plist))

  (export dictionary
          dictionary->plist
          plist->dictionary
          dictionary->alist
          alist->dictionary))
