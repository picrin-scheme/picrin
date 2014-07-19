;;; Hygienic Macros

(define-library (picrin macro)
  (import (scheme base)
          (picrin dictionary))

  ;; assumes no derived expressions are provided yet

  (define (list->vector list)
    (define vector (make-vector (length list)))
    (define (go list i)
      (if (null? list)
          vector
          (begin
            (vector-set! vector i (car list))
            (go (cdr list) (+ i 1)))))
    (go list 0))

  (define (vector->list vector)
    (define (go i)
      (if (= i (vector-length vector))
          '()
          (cons (vector-ref vector i)
                (go (+ i 1)))))
    (go 0))

  (define (vector-map proc expr)
    (list->vector (map proc (vector->list expr))))

  (define (walk proc expr)
    (if (null? expr)
        '()
        (if (pair? expr)
            (cons (walk proc (car expr))
                  (walk proc (cdr expr)))
            (if (vector? expr)
                (vector-map proc expr)
                (proc expr)))))

  (define (make-syntactic-closure env free form)
    (define cache (make-dictionary))
    (walk
     (lambda (atom)
       (if (not (symbol? atom))
           atom
           (if (memq atom free)
               atom
               (if (dictionary-has? cache atom)
                   (dictionary-ref cache atom)
                   (begin
                     (define id (make-identifier atom env))
                     (dictionary-set! cache atom id)
                     id)))))
     form))

  (define (close-syntax form env)
    (make-syntactic-closure env '() form))

  (define-syntax capture-syntactic-environment
    (lambda (form use-env mac-env)
      (list (cadr form) (list (make-identifier 'quote mac-env) mac-env))))

  (define (sc-macro-transformer f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env))))

  (define (rsc-macro-transformer f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure use-env '() (f expr mac-env))))

  (define (er-macro-transformer f)
    (lambda (expr use-env mac-env)

      (define cache (make-dictionary))

      (define (rename sym)
        (if (dictionary-has? cache sym)
            (dictionary-ref cache sym)
            (begin
              (define id (make-identifier sym mac-env))
              (dictionary-set! cache sym id)
              id)))

      (define (compare x y)
        (if (not (symbol? x))
            #f
            (if (not (symbol? y))
                #f
                (identifier=? use-env x use-env y))))

      (f expr rename compare)))

  (define (ir-macro-transformer f)
    (lambda (expr use-env mac-env)

      (define protects (make-dictionary))

      (define (wrap expr)
        (walk
         (lambda (atom)
           (if (not (symbol? atom))
               atom
               (begin
                 (define id (make-identifier atom use-env))
                 (dictionary-set! protects id atom) ; lookup *atom* from id
                 id)))
         expr))

      (define (unwrap expr)
        (define cache (make-dictionary))
        (walk
         (lambda (atom)
           (if (not (symbol? atom))
               atom
               (if (dictionary-has? protects atom)
                   (dictionary-ref protects atom)
                   (if (dictionary-has? cache atom)
                       (dictionary-ref cache atom)
                       (begin
                         ;; implicit renaming
                         (define id (make-identifier atom mac-env))
                         (dictionary-set! cache atom id)
                         id)))))
         expr))

      (define cache (make-dictionary))

      (define (inject sym)
        (if (dictionary-has? cache sym)
            (dictionary-ref cache sym)
            (begin
              (define id (make-identifier sym use-env))
              (dictionary-set! cache sym id)
              id)))

      (define (compare x y)
        (if (not (symbol? x))
            #f
            (if (not (symbol? y))
                #f
                (identifier=? mac-env x mac-env y))))

      (unwrap (f (wrap expr) inject compare))))

  (export make-syntactic-closure
          close-syntax
          capture-syntactic-environment
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer
          ir-macro-transformer))
