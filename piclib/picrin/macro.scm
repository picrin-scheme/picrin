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
    "walk on symbols"
    (if (null? expr)
        '()
        (if (pair? expr)
            (cons (walk proc (car expr))
                  (walk proc (cdr expr)))
            (if (vector? expr)
                (list->vector (walk proc (vector->list expr)))
                (if (symbol? expr)
                    (proc expr)
                    expr)))))

  (define (memoize f)
    "memoize on a symbol"
    (define cache (make-dictionary))
    (lambda (sym)
      (if (dictionary-has? cache sym)
          (dictionary-ref cache sym)
          (begin
            (define val (f sym))
            (dictionary-set! cache sym val)
            val))))

  (define (make-syntactic-closure env free form)

    (define resolve
      (memoize
       (lambda (sym)
         (make-identifier sym env))))

    (walk
     (lambda (sym)
       (if (memq sym free)
           sym
           (resolve sym)))
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

      (define rename
        (memoize
         (lambda (sym)
           (make-identifier sym mac-env))))

      (define (compare x y)
        (if (not (symbol? x))
            #f
            (if (not (symbol? y))
                #f
                (identifier=? use-env x use-env y))))

      (f expr rename compare)))

  (define (ir-macro-transformer f)
    (lambda (expr use-env mac-env)

      (define icache* (make-dictionary))

      (define inject
        (memoize
         (lambda (sym)
           (define id (make-identifier sym use-env))
           (dictionary-set! icache* id sym)
           id)))

      (define rename
        (memoize
         (lambda (sym)
           (make-identifier sym mac-env))))

      (define (compare x y)
        (if (not (symbol? x))
            #f
            (if (not (symbol? y))
                #f
                (identifier=? mac-env x mac-env y))))

      (walk (lambda (sym)
              (if (dictionary-has? icache* sym)
                  (dictionary-ref icache* sym)
                  (rename sym)))
            (f (walk inject expr) inject compare))))

  (export make-syntactic-closure
          close-syntax
          capture-syntactic-environment
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer
          ir-macro-transformer))
