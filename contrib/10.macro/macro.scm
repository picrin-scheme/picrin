(define-library (picrin macro)
  (import (picrin base))

  ;; macro primitives

  (export define-macro
          make-identifier
          identifier?
          identifier=?
          identifier-base
          identifier-environment)

  ;; simple macro

  (export define-syntax
          syntax-quote
          syntax-quasiquote
          syntax-unquote
          syntax-unquote-splicing)

  ;; misc transformers

  (export call-with-current-environment
          make-syntactic-closure
          close-syntax
          strip-syntax
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer
          ir-macro-transformer)


  (define-macro call-with-current-environment
    (lambda (form env)
      `(,(cadr form) ',env)))


  ;; syntactic closure


  (define (make-syntactic-closure env free form)
    (letrec
        ((wrap (let ((ephemeron (make-ephemeron)))
                 (lambda (var)
                   (let ((id (ephemeron var)))
                     (if id
                         (cdr id)
                         (let ((id (make-identifier var env)))
                           (ephemeron var id)
                           id))))))
         (walk (lambda (f form)
                 (cond
                  ((identifier? form)
                   (f form))
                  ((pair? form)
                   (cons (walk f (car form)) (walk f (cdr form))))
                  ((vector? form)
                   (list->vector (walk f (vector->list form))))
                  (else
                   form)))))
      (letrec
          ((f (lambda (var)
                (let loop ((free free))
                  (if (null? free)
                      (wrap var)
                      (if (identifier=? var (car free))
                          var
                          (loop (cdr free))))))))
        (walk f form))))

  (define (close-syntax form env)
    (make-syntactic-closure env '() form))

  (define (strip-syntax form)
    (letrec
        ((unwrap (lambda (var)
                   (identifier-base var)))
         (walk (lambda (f form)
                 (cond
                  ((identifier? form)
                   (f form))
                  ((pair? form)
                   (cons (walk f (car form)) (walk f (cdr form))))
                  ((vector? form)
                   (list->vector (walk f (vector->list form))))
                  (else
                   form)))))
      (walk unwrap form)))


  ;; transformers


  (define (sc-transformer f)
    (lambda (form use-env mac-env)
      (make-syntactic-closure mac-env '() (f form use-env))))

  (define (rsc-transformer f)
    (lambda (form use-env mac-env)
      (make-syntactic-closure use-env '() (f form mac-env))))

  (define (er-transformer f)
    (lambda (form use-env mac-env)
      (letrec
          ((rename (let ((ephemeron (make-ephemeron)))
                     (lambda (var)
                       (let ((id (ephemeron var)))
                         (if id
                             (cdr id)
                             (let ((id (make-identifier var mac-env)))
                               (ephemeron var id)
                               id))))))
           (compare (lambda (x y)
                      (identifier=?
                       (make-identifier x use-env)
                       (make-identifier y use-env)))))
        (f form rename compare))))

  (define (ir-transformer f)
    (lambda (form use-env mac-env)
      (let ((ephemeron1 (make-ephemeron))
            (ephemeron2 (make-ephemeron)))
        (letrec
            ((inject (lambda (var1)
                       (let ((var2 (ephemeron1 var1)))
                         (if var2
                             (cdr var2)
                             (let ((var2 (make-identifier var1 use-env)))
                               (ephemeron1 var1 var2)
                               (ephemeron2 var2 var1)
                               var2)))))
             (rename (let ((ephemeron (make-ephemeron)))
                       (lambda (var)
                         (let ((id (ephemeron var)))
                           (if id
                               (cdr id)
                               (let ((id (make-identifier var mac-env)))
                                 (ephemeron var id)
                                 id))))))
             (flip (lambda (var2) ; unwrap if injected, wrap if not injected
                     (let ((var1 (ephemeron2 var2)))
                       (if var1
                           (cdr var1)
                           (rename var2)))))
             (walk (lambda (f form)
                     (cond
                      ((identifier? form)
                       (f form))
                      ((pair? form)
                       (cons (walk f (car form)) (walk f (cdr form))))
                      ((vector? form)
                       (list->vector (walk f (vector->list form))))
                      (else
                       form))))
             (compare (lambda (x y)
                        (identifier=?
                         (make-identifier x mac-env)
                         (make-identifier y mac-env)))))
          (walk flip (f (walk inject form) inject compare))))))

  (define-macro sc-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((sc-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro rsc-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((rsc-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro er-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((er-transformer #,(cadr f)) form use-env #,mac-env))))

  (define-macro ir-macro-transformer
    (lambda (f mac-env)
      #`(lambda (form use-env)
          ((ir-transformer #,(cadr f)) form use-env #,mac-env)))))
