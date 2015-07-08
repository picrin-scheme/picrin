(define-library (picrin array)
  (import (scheme base))

  (define-record-type <array>
    (create-array data size head tail)
    array?
    (data array-data set-array-data!)
    (size array-size set-array-size!)
    (head array-head set-array-head!)
    (tail array-tail set-array-tail!))

  (define (translate ary i)
    (floor-remainder i (array-size ary)))

  (define (array-length ary)
    (let ((size (- (array-tail ary) (array-head ary))))
     (translate ary size)))

  (define (array-rotate! ary)
    (when (< (array-tail ary) (array-head ary))
      (let ((xs (vector-copy (array-data ary) 0 (array-head ary)))
            (ys (vector-copy (array-data ary) (array-head ary))))
        (set-array-data! ary (vector-append ys xs))
        (set-array-tail! ary (array-length ary))
        (set-array-head! ary 0))))

  (define (array-reserve! ary size)
    (set! size (+ size 1))              ; capa == size - 1
    (when (< (array-size ary) size)
      (array-rotate! ary)
      (set-array-data! ary (vector-append
                            (array-data ary)
                            (make-vector (- size (array-size ary)))))
      (set-array-size! ary size)))

  (define (make-array . rest)
    (if (null? rest)
        (make-array 0)
        (let ((capacity (car rest))
              (ary (create-array (make-vector 0) 0 0 0)))
          (array-reserve! ary capacity)
          ary)))

  (define (array-ref ary i)
    (let ((data (array-data ary)))
      (vector-ref data (translate ary (+ (array-head ary) i)))))

  (define (array-set! ary i obj)
    (let ((data (array-data ary)))
      (vector-set! data (translate ary (+ (array-head ary) i)) obj)))

  (define (array-push! ary obj)
    (array-reserve! ary (+ (array-length ary) 1))
    (array-set! ary (array-length ary) obj)
    (set-array-tail! ary (translate ary (+ (array-tail ary) 1))))

  (define (array-pop! ary)
    (set-array-tail! ary (translate ary (- (array-tail ary) 1)))
    (array-ref ary (array-length ary)))

  (define (array-shift! ary)
    (set-array-head! ary (translate ary (+ (array-head ary) 1)))
    (array-ref ary -1))

  (define (array-unshift! ary obj)
    (array-reserve! ary (+ (array-length ary) 1))
    (array-set! ary -1 obj)
    (set-array-head! ary (translate ary (- (array-head ary) 1))))

  (define (array->list ary)
    (do ((i 0 (+ i 1))
         (x '() (cons (array-ref ary i) x)))
        ((= i (array-length ary))
         (reverse x))))

  (define (list->array list)
    (let ((ary (make-array)))
      (for-each (lambda (x) (array-push! ary x)) list)
      ary))

  (define (array . objs)
    (list->array objs))

  (define (array-map proc ary)
    (list->array (map proc (array->list ary))))

  (define (array-for-each proc ary)
    (for-each proc (array->list ary)))

  (export make-array
          array
          array?
          array-length
          array-ref
          array-set!
          array-push!
          array-pop!
          array-shift!
          array-unshift!
          array-map
          array-for-each
          array->list
          list->array))
