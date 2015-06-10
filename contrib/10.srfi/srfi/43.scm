(define-library (srfi 43)
  (import (except (scheme base) vector-map)
          (srfi 8))
  
  ;; # Constructors
  (define (vector-unfold f length . seeds)
    (let ((seeds (if (null? seeds) '(0) seeds))
          (vect (make-vector length)))
      (letrec ((tabulate
                (lambda (count . args)
                  (if (= length count)
                      vect
                      (receive lst (apply f count args)
                               (vector-set! vect count (car lst))
                               (apply tabulate (+ 1 count) (cdr lst)))))))
        (apply tabulate 0 seeds))))

  (define (vector-unfold-right f length . seeds)
    (let ((seeds (if (null? seeds) '(0) seeds))
          (vect (make-vector length)))
      (letrec ((tabulate
                (lambda (count . args)
                  (if (< count 0)
                      vect
                      (receive lst (apply f count args)
                               (vector-set! vect count (car lst))
                               (apply tabulate (- count 1) (cdr lst)))))))
        (apply tabulate (- length 1) seeds))))

  (define (vector-reverse-copy vec . rst)
    (let* ((start (if (null? rst) 0 (car rst)))
           (end (if (or (null? rst) (null? (cdr rst)))
                    (vector-length vec)
                    (cadr rst)))
           (new-vect (make-vector (- end start))))
      (let loop ((i (- end 1)) (count 0))
        (if (< i start)
            new-vect
            (begin
              (vector-set! new-vect count (vector-ref vec i))
              (loop (- i 1) (+ 1 count)))))))

  (define (vector-concatenate list-of-vectors)
    (apply vector-append list-of-vectors))

  
  ;; # Predicates
  (define (vector-empty? vec)
    (zero? (vector-length vec)))

                                        ; for the symmetry, this should be rather 'vector=?' than 'vector='.
  (define (vector= elt=? . vects)
    (letrec ((vector2=
              (lambda (v1 v2)
                (let ((ln1 (vector-length v1)))
                  (and (= ln1 (vector-length v2))
                       (let loop ((count 0))
                         (if (= ln1 count)
                             #t
                             (and (elt=? (vector-ref v1 count)
                                         (vector-ref v2 count))
                                  (loop (+ 1 count))))))))))
      (or (null? vects)
          (let rec1 ((vect1 (car vects)) (others (cdr vects)))
            (or (null? others)
                (let ((vect2 (car others))
                      (others (cdr others)))
                  (if (eq? vect1 vect2)
                      (rec1 vect1 others)
                      (and (vector2= vect1 vect2)
                           (rec1 vect2 others)))))))))

  
  ;; # Iteration
  (define (vector-fold kons knil vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects))))
      (let rec ((acc knil) (count 0))
        (if (= count veclen)
            acc
            (rec (apply kons count acc
                        (map (lambda (v) (vector-ref v count)) vects))
                 (+ 1 count))))))

  (define (vector-fold-right kons knil vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects))))
      (let rec ((acc knil) (count (- veclen 1)))
        (if (< count 0)
            acc
            (rec (apply kons count acc
                        (map (lambda (v) (vector-ref v count)) vects))
                 (- count 1))))))

  (define (vector-map f vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects)))
           (new-vect (make-vector veclen)))
      (let rec ((count 0))
        (if (= count veclen)
            new-vect
            (begin
              (vector-set! new-vect count
                           (apply f count (map (lambda (v) (vector-ref v count))
                                               vects)))
              (rec (+ 1 count)))))))

  (define (vector-map! f vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects))))
      (let rec ((count 0))
        (if (< count veclen)
            (begin
              (vector-set! vec count
                           (apply f count (map (lambda (v) (vector-ref v count))
                                               vects)))
              (rec (+ 1 count)))))))

  (define (vector-count pred? vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects))))
      (let rec ((i 0) (count 0))
        (if (= i veclen)
            count
            (if (apply pred? count (map (lambda (v) (vector-ref v count)) vects))
                (rec (+ 1 i) (+ 1 count))
                (rec (+ 1 i) count))))))

  ;; # Searching
  (define (vector-index pred? vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (apply min (map vector-length vects))))
      (let rec ((count 0))
        (cond
         ((= count veclen) #f)
         ((apply pred? (map (lambda (v) (vector-ref v count)) vects))
          count)
         (else (rec (+ 1 count)))))))

  (define (vector-index-right pred? vec . vects)
    (let ((vects (cons vec vects))
          (veclen (vector-length vec)))
      (let rec ((count (- veclen 1)))
        (cond
         ((< count 0) #f)
         ((apply pred? (map (lambda (v) (vector-ref v count)) vects))
          count)
         (else (rec (- count 1)))))))

  (define (vector-skip pred? vec . vects)
    (apply vector-index (lambda args (not (apply pred? args))) vec vects))

  (define (vector-skip-right pred? vec . vects)
    (apply vector-index-right (lambda args (not (apply pred? args))) vec vects))

  (define (vector-binary-search vec value cmp)
    (let rec ((start 0) (end (vector-length vec)) (n -1))
      (let ((count (floor/ (+ start end) 2)))
        (if (or (= start end) (= count n))
            #f
            (let ((comparison (cmp (vector-ref vec count) value)))
              (cond
               ((zero? comparison) count)
               ((positive? comparison) (rec start count count))
               (else (rec count end count))))))))

  (define (vector-any pred? vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (vector-length vec)))
      (let rec ((count 0))
        (if (= count veclen)
            #f
            (or (apply pred? (map (lambda (v) (vector-ref v count)) vects))
                (rec (+ 1 count)))))))

  (define (vector-every pred? vec . vects)
    (let* ((vects (cons vec vects))
           (veclen (vector-length vec)))
      (let rec ((count 0))
        (if (= count veclen)
            #t
            (and (apply pred? (map (lambda (v) (vector-ref v count)) vects))
                 (rec (+ 1 count)))))))

  ;; # Mutators
  (define (vector-swap! vec i j)
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)))

  (define (vector-reverse! vec . rst)
    (let ((start (if (null? rst) 0 (car rst)))
          (end (if (or (null? rst) (cdr rst))
                   (vector-length vec)
                   (cadr rst))))
      (let rec ((i start) (j (- end 1)))
        (if (< i j)
            (begin
              (vector-swap! vec i j)
              (rec (+ 1 i) (- j 1)))))))

  (define (vector-reverse-copy! target tstart source . rst)
    (let ((sstart (if (null? rst) 0 (car rst)))
          (send (if (or (null? rst) (cdr rst))
                    (vector-length source)
                    (cadr rst))))
      (let rec ((i tstart) (j (- send 1)))
        (if (>= j sstart)
            (begin
              (vector-set! target i (vector-ref source j))
              (rec (+ 1 i) (- j 1)))))))

  ;; # Conversion
  (define (reverse-vector->list vec . rst)
    (let ((start (if (null? rst) 0 (car rst)))
          (end (if (or (null? rst) (cdr rst))
                   (vector-length vec)
                   (cadr rst))))
      (let rec ((i start) (acc '()))
        (if (= i end)
            acc
            (rec (+ 1 i) (cons (vector-ref vec i) acc))))))

  (define (reverse-list->vector proper-list)
    (apply vector (reverse proper-list)))

  (export vector?
          make-vector
          vector
          vector-length
          vector-ref
          vector-set!
          vector->list
          list->vector
          vector-fill!
          vector-copy!

          vector-unfold
          vector-unfold-right
          vector-reverse-copy
          vector-concatenate
          vector-empty?
          vector=
          vector-fold
          vector-fold-right
          vector-map!
          vector-count
          vector-index
          vector-index-right
          vector-skip
          vector-skip-right
          vector-binary-search
          vector-any
          vector-every
          vector-swap!
          vector-reverse!
          vector-reverse-copy!
          reverse-vector->list
          reverse-list->vector))
