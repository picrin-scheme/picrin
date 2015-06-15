(define-library (picrin syntax-rules)
  (import (picrin base)
          (picrin macro))

  (define-syntax (define-auxiliary-syntax var)
    #`(define-macro #,var
        (lambda _
          (error "invalid use of auxiliary syntax" '#,var))))

  (define-auxiliary-syntax _)
  (define-auxiliary-syntax ...)

  (define (succ n)
    (+ n 1))

  (define (pred n)
    (if (= n 0)
        0
        (- n 1)))

  (define (every? args)
    (if (null? args)
        #t
        (if (car args)
            (every? (cdr args))
            #f)))

  (define (filter f list)
    (if (null? list)
        '()
        (if (f (car list))
            (cons (car list)
                  (filter f (cdr list)))
            (filter f (cdr list)))))

  (define (map-keys f assoc)
    (map (lambda (s) `(,(f (car s)) . ,(cdr s))) assoc))

  (define (map-values f assoc)
    (map (lambda (s) `(,(car s) . ,(f (cdr s)))) assoc))

  ;; TODO
  ;; - splicing
  ;; - placeholder
  ;; - vector

  ;; p ::= constant
  ;;     | var
  ;;     | (p . p)
  ;;     | (p ...)

  ;; only template supports (p ... . p) pattern
  ;; tp := constant
  ;;     | var
  ;;     | (p . p)
  ;;     | (p ... . p)

  (define (compile ellipsis literals rules)

    (define (constant? obj)
      (and (not (pair? obj))
           (not (variable? obj))))

    (define (literal? obj)
      (and (variable? obj)
           (memq obj literals)))

    (define (many? pat)
      (and (pair? pat)
           (pair? (cdr pat))
           (variable? (cadr pat))
           (variable=? (cadr pat) ellipsis)))

    (define (pattern-validator pat)      ; pattern -> validator
      (letrec
          ((pattern-validator
            (lambda (pat form)
              (cond
               ((constant? pat)
                #`(equal? '#,pat #,form))
               ((literal? pat)
                #`(variable=? #'#,pat #,form))
               ((variable? pat)
                #t)
               ((many? pat)
                (let ((validator (pattern-validator (car pat) 'it)))
                  #`(and (list? #,form)
                         (every? (map (lambda (#,'it) #,validator) #,form)))))
               ((pair? pat)
                #`(and (pair? #,form)
                       #,(pattern-validator (car pat) #`(car #,form))
                       #,(pattern-validator (cdr pat) #`(cdr #,form))))
               (else
                #f)))))
        (pattern-validator pat 'it)))

    (define (pattern-variables pat)       ; pattern -> (freevar)
      (cond
       ((constant? pat)
        '())
       ((literal? pat)
        '())
       ((variable? pat)
        `(,pat))
       ((many? pat)
        (pattern-variables (car pat)))
       ((pair? pat)
        (append (pattern-variables (car pat))
                (pattern-variables (cdr pat))))))

    (define (pattern-levels pat)          ; pattern -> ((var * int))
      (cond
       ((constant? pat)
        '())
       ((literal? pat)
        '())
       ((variable? pat)
        `((,pat . 0)))
       ((many? pat)
        (map-values succ (pattern-levels (car pat))))
       ((pair? pat)
        (append (pattern-levels (car pat))
                (pattern-levels (cdr pat))))))

    (define (pattern-selectors pat)       ; pattern -> ((var * selector))
      (letrec
          ((pattern-selectors
            (lambda (pat form)
              (cond
               ((constant? pat)
                '())
               ((literal? pat)
                '())
               ((variable? pat)
                `((,pat . ,form)))
               ((many? pat)
                (let ((envs (pattern-selectors (car pat) 'it)))
                  (map-values (lambda (s) #`(map (lambda (#,'it) #,s) #,form)) envs)))
               ((pair? pat)
                (append (pattern-selectors (car pat) #`(car #,form))
                        (pattern-selectors (cdr pat) #`(cdr #,form))))))))
        (pattern-selectors pat 'it)))

    (define (template-representation pat levels selectors)
      (cond
       ((constant? pat)
        pat)
       ((variable? pat)
        (let ((it (assq pat levels)))
          (if it
              (if (= 0 (cdr it))
                  (cdr (assq pat selectors))
                  (error "unmatched pattern variable level" pat))
              #`(#,'rename '#,pat))))
       ((many? pat)
        (letrec*
            ((inner-pat
              (car pat))
             (inner-levels
              (map (lambda (s) `(,(car s) . ,(pred (cdr s)))) levels))
             (inner-freevars
              (filter (lambda (v) (assq v levels)) (pattern-variables inner-pat)))
             (inner-vars
              ;; select only vars declared with ellipsis
              (filter (lambda (v) (> (cdr (assq v levels)) 0)) inner-freevars))
             (inner-tmps
              (map (lambda (v) #'it) inner-vars))
             (inner-selectors
              ;; first env '(map cons ...)' shadows second env 'selectors'
              (append (map cons inner-vars inner-tmps) selectors))
             (inner-rep
              (template-representation inner-pat inner-levels inner-selectors))
             (sorted-selectors
              (map (lambda (v) (assq v selectors)) inner-vars))
             (list-of-selectors
              ;; ((a . xs) (b . ys) (c . zs)) -> (xs ys zs)
              (map cdr sorted-selectors)))
          (let ((rep1 #`(map (lambda #,inner-tmps #,inner-rep) #,@list-of-selectors))
                (rep2 (template-representation (cddr pat) levels selectors)))
            #`(append #,rep1 #,rep2))))
       ((pair? pat)
        #`(cons #,(template-representation (car pat) levels selectors)
                #,(template-representation (cdr pat) levels selectors)))))

    (define (compile-rule pattern template)
      (let ((levels
             (pattern-levels pattern))
            (selectors
             (pattern-selectors pattern)))
        (template-representation template levels selectors)))

    (define (compile-rules rules)
      (if (null? rules)
          #`(error "unmatch")
          (let ((pattern (car (car rules)))
                (template (cadr (car rules))))
            #`(if #,(pattern-validator pattern)
                  #,(compile-rule pattern template)
                  #,(compile-rules (cdr rules))))))

    (define (compile rules)
      #`(call-with-current-environment
         (lambda (env)
           (letrec
               ((#,'rename (let ((reg (make-register)))
                             (lambda (x)
                               (if (undefined? (reg x))
                                   (let ((id (make-identifier x env)))
                                     (reg x id)
                                     id)
                                   (reg x))))))
             (lambda #,'it
               #,(compile-rules rules))))))

    (let ((rules (map-keys cdr rules))) ; TODO: check pattern head is a variable
      (compile rules)))

  (define-syntax (syntax-rules . args)
    (if (list? (car args))
        #`(syntax-rules ... #,@args)
        (let ((ellipsis (car args))
              (literals (car (cdr args)))
              (rules    (cdr (cdr args))))
          (compile ellipsis literals rules))))


  (export syntax-rules
          _
          ...))
