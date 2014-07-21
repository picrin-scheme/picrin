(define-library (srfi 95)
  (import (scheme base)
          (scheme load)
          (srfi 1))

  (define (list-sorted? ls less?)
    (let loop ((cur ls))
      (if (<= (length cur) 1)
          #t
          (if (less? (second cur) (first cur))
              #f
              (loop (cdr cur))))))

  (define (identity x)
    x)

  (define (merge ls1 ls2 less? . opt-key)
    (let ((key (if (null? opt-key) identity (car opt-key))))
      (let rec ((arg1 ls1) (arg2 ls2))
        (cond ((null? arg1)
               arg2)
              ((null? arg2)
               arg1)
              ((less? (key (car arg1)) (key (car arg2)))
               (cons (car arg1) (rec (cdr arg1) arg2)))
              (else
               (cons (car arg2) (rec arg1 (cdr arg2))))))))

  (define (merge-sub! ls1 ls2 less? key)
    (let rec ((arg1 ls1) (arg2 ls2))
      (cond ((null? arg1)
             arg2)
            ((null? arg2)
             arg1)
            ((not (less? (key (car arg2)) (key (car arg1))))
             (set-cdr! arg1 (rec (cdr arg1) arg2)) arg1)
            (else
             (set-cdr! arg2 (rec arg1 (cdr arg2))) arg2))))

  (define (merge! ls1 ls2 less? . opt-key)
    (let ((key (if (null? opt-key) identity (car opt-key)))
          (c1 (car ls1))
          (c2 (car ls2))
          (d1 (cdr ls1))
          (d2 (cdr ls2)))
      (when (less? (key c2) (key c1))
        (set-car! ls1 c2)
        (set-car! ls2 c1)
        (set-cdr! ls1 d2)
        (set-cdr! ls2 d1))
      (merge-sub! ls1 ls2 less? key)))

  (define (merge-sort ls less?)
       (if (<= (length ls) 1)
           ls
           (let* ((n (length ls))
                  (p (quotient n 2))
                  (as (take ls p))
                  (bs (drop ls p))
                  (sa (merge-sort as less?))
                  (sb (merge-sort bs less?)))
             (merge sa sb less?))))

  (define (merge-sort! ls less?)
    (if (<= (length ls) 1) ls
        (let* ((n (length ls))
               (p (quotient n 2))
               (bs (drop ls p))
               (as (take! ls p))
               (sa (merge-sort! as less?))
               (sb (merge-sort! bs less?)))
          (merge! sa sb less?))))

  (export list-sorted?
          merge
          merge!
          merge-sort
          merge-sort!))
