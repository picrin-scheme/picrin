(begin
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
        name                              ; TODO: check symbol names
        (string->symbol (join (map ->string name) "."))))

  (define current-library
    (make-parameter '(picrin base) mangle))

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
            (add-macro! name macro))))

    (define-transformer 'define-library
      (lambda (form _)
        (let ((name (cadr form))
              (body (cddr form)))
          (or (find-library name) (make-library name))
          (parameterize ((current-library name))
            (for-each
             (lambda (expr)
               (eval expr name))       ; TODO parse library declarations
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
    (let ((export-keywords
           (lambda (keywords)
             (let ((env (library-environment '(picrin base)))
                   (exports (library-exports '(picrin base))))
               (for-each
                (lambda (keyword)
                  (dictionary-set! exports keyword keyword))
                keywords)))))
      (export-keywords
       '(define lambda quote set! if begin define-macro
          let let* letrec letrec*
          let-values let*-values define-values
          quasiquote unquote unquote-splicing
          and or
          cond case else =>
          do when unless
          parameterize
          define-syntax
          syntax-quote syntax-unquote
          syntax-quasiquote syntax-unquote-splicing
          let-syntax letrec-syntax
          syntax-error))
      (export-keywords
       '(features
         eq? eqv? equal? not boolean? boolean=?
         pair? cons car cdr null? set-car! set-cdr!
         caar cadr cdar cddr
         list? make-list list length append reverse
         list-tail list-ref list-set! list-copy
         map for-each memq memv member assq assv assoc
         current-input-port current-output-port current-error-port
         port? input-port? output-port? port-open? close-port
         eof-object? eof-object
         read-u8 peek-u8 read-bytevector!
         write-u8 write-bytevector flush-output-port
         open-input-bytevector open-output-bytevector get-output-bytevector
         number? exact? inexact? inexact exact
         = < > <= >= + - * /
         number->string string->number
         procedure? apply
         symbol? symbol=? symbol->string string->symbol
         make-identifier identifier? identifier=? identifier-base identifier-environment
         vector? vector make-vector vector-length vector-ref vector-set!
         vector-copy! vector-copy vector-append vector-fill! vector-map vector-for-each
         list->vector vector->list string->vector vector->string
         bytevector? bytevector make-bytevector
         bytevector-length bytevector-u8-ref bytevector-u8-set!
         bytevector-copy! bytevector-copy bytevector-append
         bytevector->list list->bytevector
         call-with-current-continuation call/cc values call-with-values
         char? char->integer integer->char char=? char<? char>? char<=? char>=?
         current-exception-handlers with-exception-handler
         raise raise-continuable error
         error-object? error-object-message error-object-irritants
         error-object-type
         string? string make-string string-length string-ref string-set!
         string-copy string-copy! string-fill! string-append
         string-map string-for-each list->string string->list
         string=? string<? string>? string<=? string>=?
         make-parameter with-dynamic-environment
         read
         make-dictionary dictionary? dictionary dictionary-has?
         dictionary-ref dictionary-set! dictionary-delete! dictionary-size
         dictionary-map dictionary-for-each
         dictionary->alist alist->dictionary dictionary->plist plist->dictionary
         make-record record? record-type record-datum
         default-environment make-environment find-identifier set-identifier!
         eval compile add-macro!
         make-ephemeron-table
         write write-simple write-shared display))
      (export-keywords
       '(find-library make-library current-library)))
    (set! eval
          (let ((e eval))
            (lambda (expr . lib)
              (let ((lib (if (null? lib) (current-library) (car lib))))
                (e expr (library-environment lib))))))
    (make-library '(picrin user))
    (current-library '(picrin user))))
