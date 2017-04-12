(define-values (current-library
                find-library
                make-library
                library-environment
                library-exports
                library-import
                library-export)
  (let ()
    ;; There are two ways to name a library: (foo bar) or foo.bar
    ;; The former is normalized to the latter.

    (define (mangle name)
      (when (null? name)
        (error "library name should be a list of at least one symbols" name))

      (define (->string n)
        (cond
         ((symbol? n)
          (let ((str (symbol->string n)))
            (string-for-each
             (lambda (c)
               (when (or (char=? c #\.) (char=? c #\:))
                 (error "elements of library name may not contain '.' or ':'" n)))
             str)
            str))
         ((and (number? n) (exact? n) (<= 0 n))
          (number->string n))
         (else
          (error "symbol or non-negative integer is required" n))))

      (define (join strs delim)
        (let loop ((res (car strs)) (strs (cdr strs)))
          (if (null? strs)
              res
              (loop (string-append res delim (car strs)) (cdr strs)))))

      (if (symbol? name)
          name                        ; TODO: check symbol names
          (string->symbol (join (map ->string name) "."))))

    (define current-library
      (make-parameter '(picrin user) mangle))

    (define *libraries*
      (make-dictionary))

    (define (find-library name)
      (dictionary-has? *libraries* (mangle name)))

    (define (make-library name)
      (let ((name (mangle name)))
        (let ((env (make-environment
                     (string->symbol (string-append (symbol->string name) ":"))))
              (exports (make-dictionary)))
          ;; set up initial environment
          (set-identifier! 'define-library 'define-library env)
          (set-identifier! 'import 'import env)
          (set-identifier! 'export 'export env)
          (set-identifier! 'cond-expand 'cond-expand env)
          (dictionary-set! *libraries* name `(,env . ,exports)))))

    (define (library-environment name)
      (car (dictionary-ref *libraries* (mangle name))))

    (define (library-exports name)
      (cdr (dictionary-ref *libraries* (mangle name))))

    (define (library-import name sym alias)
      (let ((uid (dictionary-ref (library-exports name) sym)))
        (let ((env (library-environment (current-library))))
          (set-identifier! alias uid env))))

    (define (library-export sym alias)
      (let ((env (library-environment (current-library)))
            (exports (library-exports (current-library))))
        (dictionary-set! exports alias (find-identifier sym env))))



    ;; R7RS library syntax

    (let ((define-transformer
            (lambda (name macro)
              (dictionary-set! (macro-objects) name macro))))

      (define-transformer 'define-library
        (lambda (form _)
          (let ((name (cadr form))
                (body (cddr form)))
            (or (find-library name) (make-library name))
            (parameterize ((current-library name))
              (for-each
               (lambda (expr)
                 (let ((exprs (if (and (pair? expr) (eq? (car expr) 'begin))
                                  (cdr expr)
                                  (list expr))))
                   (for-each
                    (lambda (e) (eval e name))
                    exprs)))
               body)))))

      (define-transformer 'cond-expand
        (lambda (form _)
          (letrec
              ((test (lambda (form)
                       (or
                        (eq? form 'else)
                        (and (symbol? form)
                             (memq form (features)))
                        (and (pair? form)
                             (case (car form)
                               ((library) (find-library (cadr form)))
                               ((not) (not (test (cadr form))))
                               ((and) (let loop ((form (cdr form)))
                                        (or (null? form)
                                            (and (test (car form)) (loop (cdr form))))))
                               ((or) (let loop ((form (cdr form)))
                                       (and (pair? form)
                                            (or (test (car form)) (loop (cdr form))))))
                               (else #f)))))))
            (let loop ((clauses (cdr form)))
              (if (null? clauses)
                  #undefined
                  (if (test (caar clauses))
                      `(,(make-identifier 'begin default-environment) ,@(cdar clauses))
                      (loop (cdr clauses))))))))

      (define-transformer 'import
        (lambda (form _)
          (let ((caddr
                 (lambda (x) (car (cdr (cdr x)))))
                (prefix
                 (lambda (prefix symbol)
                   (string->symbol
                    (string-append
                     (symbol->string prefix)
                     (symbol->string symbol)))))
                (getlib
                 (lambda (name)
                   (if (find-library name)
                       name
                       (error "library not found" name)))))
            (letrec
                ((extract
                  (lambda (spec)
                    (case (car spec)
                      ((only rename prefix except)
                       (extract (cadr spec)))
                      (else
                       (getlib spec)))))
                 (collect
                  (lambda (spec)
                    (case (car spec)
                      ((only)
                       (let ((alist (collect (cadr spec))))
                         (map (lambda (var) (assq var alist)) (cddr spec))))
                      ((rename)
                       (let ((alist (collect (cadr spec)))
                             (renames (map (lambda (x) `(,(car x) . ,(cadr x))) (cddr spec))))
                         (map (lambda (s) (or (assq (car s) renames) s)) alist)))
                      ((prefix)
                       (let ((alist (collect (cadr spec))))
                         (map (lambda (s) (cons (prefix (caddr spec) (car s)) (cdr s))) alist)))
                      ((except)
                       (let ((alist (collect (cadr spec))))
                         (let loop ((alist alist))
                           (if (null? alist)
                               '()
                               (if (memq (caar alist) (cddr spec))
                                   (loop (cdr alist))
                                   (cons (car alist) (loop (cdr alist))))))))
                      (else
                       (dictionary-map (lambda (x) (cons x x))
                                       (library-exports (getlib spec))))))))
              (letrec
                  ((import
                     (lambda (spec)
                       (let ((lib (extract spec))
                             (alist (collect spec)))
                         (for-each
                          (lambda (slot)
                            (library-import lib (cdr slot) (car slot)))
                          alist)))))
                (for-each import (cdr form)))))))

      (define-transformer 'export
        (lambda (form _)
          (letrec
              ((collect
                (lambda (spec)
                  (cond
                   ((symbol? spec)
                    `(,spec . ,spec))
                   ((and (list? spec) (= (length spec) 3) (eq? (car spec) 'rename))
                    `(,(list-ref spec 1) . ,(list-ref spec 2)))
                   (else
                    (error "malformed export")))))
               (export
                 (lambda (spec)
                   (let ((slot (collect spec)))
                     (library-export (car slot) (cdr slot))))))
            (for-each export (cdr form))))))


    ;; bootstrap...

    (let ()
      (make-library '(picrin base))
      (set-car! (dictionary-ref *libraries* (mangle '(picrin base))) default-environment)
      (let* ((exports
              (library-exports '(picrin base)))
             (export-keyword
              (lambda (keyword)
                (dictionary-set! exports keyword keyword))))
        (for-each export-keyword
                  '(define lambda quote set! if begin define-macro
                     let let* letrec letrec*
                     let-values let*-values define-values
                     quasiquote unquote unquote-splicing
                     and or
                     cond case else =>
                     do when unless
                     parameterize define-record-type))
        (export-keyword 'boolean?)
        (dictionary-for-each export-keyword (global-objects)))
      (set! eval
            (let ((e eval))
              (lambda (expr . lib)
                (let ((lib (if (null? lib) (current-library) (car lib))))
                  (parameterize ((current-library lib))
                    (e expr (library-environment lib)))))))
      (make-library '(picrin user)))

    (values current-library
            find-library
            make-library
            library-environment
            library-exports
            library-import
            library-export)))
