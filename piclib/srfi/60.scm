(define-library (srfi 60)
  (import (scheme base)
          (srfi 1))

  ;; # Bitwise Operations
  (define (logand . args)
    (letrec ((lgand
              (lambda (x y)
                (if (or (zero? x) (zero? y))
                    0
                    (+ (* (lgand (floor/ x 2) (floor/ y 2)) 2)
                       (if (or (even? x) (even? y)) 0 1))))))
      (fold lgand -1 args)))

  (define bitwise-and logand)

  (define (logior . args)
    (letrec ((lgior
              (lambda (x y)
                (cond
                 ((= x y) x)
                 ((zero? x) y)
                 ((zero? y) x)
                 (else
                  (+ (* (lgior (truncate-quotient x 2)
                               (truncate-quotient y 2))
                        2)
                     (if (and (even? x) (even? y)) 0 1)))))))
      (fold lgior 0 args)))

  (define bitwise-ior logior)

  (define (logxor . args)
    (letrec ((lgxor
              (lambda (x y)
                (cond
                 ((zero? x) y)
                 ((zero? y) x)
                 (else
                  (+ (* (lgxor (floor/ x 2) (floor/ y 2)) 2)
                     (if (even? x)
                         (if (even? y) 0 1)
                         (if (even? y) 1 0))))))))
      (fold lgxor 0 args)))

  (define bitwise-xor logxor)

  (define (lognot n)
    (- -1 n))

  (define bitwise-not lognot)

  (define (bitwise-if mask n0 n1)
    (logior (logand mask n0)
            (logand (lognot mask) n1)))

  (define bitwise-merge bitwise-if)

  (define (logtest j k)
    (not (zero? (logand j k))))

  (define any-bits-set? logtest)

  ;; # Integer Properties
  (define (logcount n)
    (letrec ((lgcnt
              (lambda (n)
                (if (zero? n) 0
                    (+ (lgcnt (floor/ n 2))
                       (if (even? n) 0 1))))))
      (if (negative? n)
          (lgcnt (lognot n))
          (lgcnt n))))

  (define bit-count logcount)

  (define (integer-length n)
    (let loop ((n n) (count 0))
      (if (zero? n)
          count
          (loop (floor/ n 2) (+ count 1)))))

  (define (log2-binary-factors n)
    (+ -1 (integer-length (logand n (- n)))))

  (define first-set-bit log2-binary-factors)
  
  ;; # Bit Within Word
  (define (logbit? index n)
    (logtest (expt 2 index) n))

  (define bit-set? logbit?)

  (define (copy-bit index from bit)
    (if bit
        (logior from (expt 2 index))
        (logand from (lognot (expt 2 index)))))


  ;; # Field of Bits
  (define (ash n count)
    (if (negative? count)
        (let ((k (expt 2 (- count))))
          (if (negative? n)
              (+ -1 (truncate-quotient (+ 1 n) k))
              (truncate-quotient n k)))
        (* (expt 2 count) n)))

  (define arithmetic-shift ash)

  (define (bit-field n start end)
    (logand (lognot (ash -1 (- end start)))
            (ash n (- start))))

  (define (copy-bit-field to from start end)
    (bitwise-if (ash (lognot (ash -1 (- end start))) start)
                (ash from start)
                to))

  (define (rotate-bit-field n count start end)
    (let* ((width (- start end))
           (count (floor-remainder count width))
           (mask (lognot (ash -1 width)))
           (zn (logand mask (ash n (- start)))))
      (logior (ash (logior (logand mask (ash zn count))
                           (ash zn (- count width)))
                   start)
              (logand (lognot (ash mask start)) n))))

  (define (reverse-bit-field n start end)
    (letrec ((bit-reverse
              (lambda (k n)
                (let loop ((m (if (negative? n) (lognot n) n))
                           (k (- k 1))
                           (rvs 0))
                  (if (negative? k)
                      (if (negative? n) (lognot rvs) rvs)
                      (loop (ash m -1)
                            (- k 1)
                            (logior (ash rvs 1) (logand 1 m))))))))
      (let* ((width (- start end))
             (mask (lognot (ash -1 width)))
             (zn (logand mask (ash n (- start)))))
        (logior (ash (bit-reverse width zn) start)
                (logand (lognot (ash mask start)) n)))))

  ;; Bits as Booleans
  (define (integer->list k . len)
    (let ((len (if (null? len) (integer-length k) len)))
      (let loop ((k k) (len len) (acc '()))
        (if (or (zero? k) (zero? len))
            acc
            (loop (floor/ k 2) (- len 1) (cons (if (even? k) #f #t) acc))))))

  (define (list->integer lst)
    (let loop ((lst lst) (acc 0))
      (if (null? lst)
          acc
          (loop (cdr lst) (+ (* acc 2) (if (car lst) 1 0))))))

  (define (booleans->integer . args)
    (list->integer args))

  (export logand bitwise-and
          logior bitwise-ior
          logxor bitwise-xor
          lognot bitwise-not
          bitwise-if bitwise-merge
          logtest any-bits-set?
          logcount bit-count
          integer-length
          log2-binary-factors first-set-bit
          logbit? bit-set?
          copy-bit
          bit-field
          copy-bit-field
          ash arithmetic-shift
          rotate-bit-field
          reverse-bit-field
          integer->list
          list->integer
          booleans->integer))
