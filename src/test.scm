(import (scheme base)
        (scheme write)
        (scheme case-lambda)
        (scheme cxr)
        (scheme lazy)
        (srfi 1)
        (srfi 17)
        (expand))

(define (print x)
  (write x)
  (newline)
  x)

(define-syntax p
  (syntax-rules ()
    ((_ expr)
     (begin
       (display "; ")
       (print 'expr)
       (print expr)
       (newline)))))

; test

(define default-env
  (letrec
      ((default-env
         '(()))

       (define-object
         (lambda (sym)
           (add-identifier! sym default-env)))

       (define-macro
         (lambda (sym expander)
           (let ((qid (define-object sym)))
             (add-macro! qid (expander default-env))))))

    (define-macro 'quote
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-quote (make-identifier 'quote builtin-environment)))
            `(,the-quote ,(second form))))))

    (define-macro 'define
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-define (make-identifier 'define builtin-environment))
                (the-lambda (make-identifier 'lambda mac-env)))
            (if (identifier? (second form))
                `(,the-define ,(second form) ,(third form))
                `(,the-define ,(car (second form))
                              (,the-lambda ,(cdr (second form))
                                           ,@(cddr form))))))))

    (define-macro 'lambda
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-lambda (make-identifier 'lambda builtin-environment))
                (the-begin (make-identifier 'begin mac-env)))
            `(,the-lambda ,(second form) (,the-begin . ,(cddr form)))))))

    (define-macro 'if
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-if (make-identifier 'if builtin-environment))
                (test (second form))
                (then (third form))
                (else (if (= (length form) 4) (fourth form) #f)))
            `(,the-if ,test ,then ,else)))))

    (define-macro 'begin
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-begin (make-identifier 'begin builtin-environment)))
            (cond
             ((= (length form) 1)
              #f)
             ((= (length form) 2)
              (second form))
             ((= (length form) 3)
              `(,the-begin ,(second form) ,(third form)))
             (else
              `(,the-begin ,(second form) (,(first form) . ,(cddr form)))))))))

    (define-macro 'set!
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-set! (make-identifier 'set! builtin-environment)))
            `(,the-set! ,(second form) ,(third form))))))

    (define-macro 'let
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-lambda (make-identifier 'lambda mac-env)))
            `((,the-lambda (,(second form))
                           . ,(cdddr form))
              ,(third form))))))

    (define-macro 'swap!
      (lambda (mac-env)
        (lambda (form use-env)
          (let ((the-let (make-identifier 'let mac-env))
                (the-set! (make-identifier 'set! mac-env))
                (the-tmp (make-identifier 'tmp mac-env)))
            `(,the-let ,the-tmp ,(second form)
                       (,the-set! ,(second form) ,(third form))
                       (,the-set! ,(third form) ,the-tmp))))))

    (define-object '+)
    (define-object '-)
    (define-object '<)

    default-env))

(p (expand '(let x 1 (+ x 2)) default-env))
(p (expand '(if #f #f) default-env))
(p (expand '(let x 1 (let y 2 (swap! x y))) default-env))


; library

;; (define-record-type <library>
;;     (make-library env exports)
;;     library?
;;   (env library-environment)
;;   (exports library-exports set-library-exports!))

;; (define libraries '())

;; (define current-library
;;   (make-parameter #f))

;; (define (find-library name)
;;   (cdr (assoc name libraries)))

;; (define (install-library name lib)
;;   (push! `(,name . ,lib) libraries))

;; (define (import lib prefix)
;;   (let ((imports (library-exports lib))
;;         (src-env (library-environment lib)))
;;     (let ((dest-env (library-environment (current-library))))
;;       (for-each
;;        (lambda (name)
;;          (let ((qid (find name src-env)))
;;            (let ((new-name
;;                   (string->symbol
;;                    (string-append
;;                     (symbol->string prefix)
;;                     (symbol->string name)))))
;;              (install new-name qid dest-env))))
;;        imports))))

;; (define (export sym)
;;   (set-library-exports!
;;    (current-library)
;;    (cons sym (library-exports (current-library)))))


; builtin environment & library

;; (define builtin-lib
;;   (make-library builtin-env (map car (car builtin-env))))

;; (install-library '(picrin builtin) builtin-lib)

;; (current-library builtin-lib)
