(define-library (picrin dictionary)
  (import (picrin base))

  (define (dictionary-map proc dict)
    (let ((kvs '()))
      (dictionary-for-each
       (lambda (key val)
         (set! kvs (cons (proc key val) kvs)))
       dict)
      (reverse kvs)))

  (define (dictionary->plist dict)
    (let ((kvs '()))
      (dictionary-for-each
       (lambda (key val)
         (set! kvs (cons val (cons key kvs))))
       dict)
      (reverse kvs)))

  (define (plist->dictionary plist)
    (let ((dict (make-dictionary)))
      (do ((kv plist (cddr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cadr kv)))))

  (define (dictionary->alist dict)
    (dictionary-map
     (lambda (key val)
       (cons key val))
     dict))

  (define (alist->dictionary alist)
    (let ((dict (make-dictionary)))
      (do ((kv alist (cdr kv)))
          ((null? kv)
           dict)
        (dictionary-set! dict (car kv) (cdr kv)))))

  (define (dictionary . plist)
    (plist->dictionary plist))

  (export dictionary
          dictionary-map
          dictionary->plist
          plist->dictionary
          dictionary->alist
          alist->dictionary))
