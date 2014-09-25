
(define-library (picrin documentation)
  ;; currently, you can set documentation on only procs
  
  ;; (doc quote "")
  ;; (doc lambda "")
  ;; (doc if "")
  ;; (doc set! "")
  ;; (doc include "")
  ;; (doc cond ""
  ;;      case ""
  ;;      and ""
  ;;      or ""
  ;;      when ""
  ;;      unless "")
  ;; (doc let ""
  ;;      let* ""
  ;;      letrec ""
  ;;      letrec* ""
  ;;      let-values ""
  ;;      let*-values "")
  ;; (doc begin "")
  ;; (doc do "")
  (doc make-parameter ""
       ;; parameterize ""
       )
  ;; (doc guard "")
  ;; (doc quasiquote ""
  ;;      unquote ""
  ;;      unquote-splicing "")
  ;; (doc let-syntax ""
  ;;      letrec-syntax "")
  ;; (doc syntax-rules "")
  ;; (doc syntax-error "")
  ;; (doc define "")
  ;; (doc define-values "")
  ;; (doc define-syntax "")
  ;; (doc define-record-type "")
  (doc eq? ""
       eqv? ""
       equal? "")
  (doc number? ""
       complex? ""
       real? ""
       rational? ""
       integer? ""
       exact? ""
       inexact? ""
       exact-integer? ""
       exact ""
       inexact ""
       = ""
       < ""
       > ""
       <= ""
       >= ""
       zero? ""
       positive? ""
       negative? ""
       odd? ""
       even? ""
       min ""
       max ""
       + ""
       - ""
       * ""
       / ""
       abs ""
       floor-quotient ""
       floor-remainder ""
       floor/ ""
       truncate-quotient ""
       truncate-remainder ""
       truncate/ ""
       ;; (rename truncate-quotient quotient)
       ;; (rename truncate-remainder remainder)
       ;; (rename floor-remainder modulo)
       gcd ""
       lcm ""
       floor ""
       ceiling ""
       truncate ""
       round ""
       exact-integer-sqrt ""
       square ""
       expt ""
       number->string ""
       string->number "")
  (doc boolean? ""
       boolean=? ""
       not "")
  (doc pair? ""
       cons ""
       car ""
       cdr ""
       set-car! ""
       set-cdr! ""
       null? ""
       caar ""
       cadr ""
       cdar ""
       cddr ""
       list? ""
       make-list ""
       list ""
       length ""
       append ""
       reverse ""
       list-tail ""
       list-ref ""
       list-set! ""
       list-copy ""
       memq ""
       memv ""
       member ""
       assq ""
       assv ""
       assoc "")
  (doc symbol? ""
       symbol=? ""
       symbol->string ""
       string->symbol "")
  (doc char? ""
       char->integer ""
       integer->char ""
       char=? ""
       char<? ""
       char>? ""
       char<=? ""
       char>=? "")
  (doc string? ""
       string ""
       make-string ""
       string-length ""
       string-ref ""
       string-set! ""
       string-copy ""
       string-copy! ""
       string-append ""
       ;; (rename string-copy substring)
       string-fill! ""
       string->list ""
       list->string ""
       string=? ""
       string<? ""
       string>? ""
       string<=? ""
       string>=? "")
  (doc vector? ""
       vector ""
       make-vector ""
       vector-length ""
       vector-ref ""
       vector-set! ""
       vector-copy! ""
       vector-copy ""
       vector-append ""
       vector-fill! ""
       list->vector ""
       vector->list ""
       string->vector ""
       vector->string "")
  (doc bytevector? ""
       bytevector ""
       make-bytevector ""
       bytevector-length ""
       bytevector-u8-ref ""
       bytevector-u8-set! ""
       bytevector-copy ""
       bytevector-copy! ""
       bytevector-append ""
       bytevector->list ""
       list->bytevector ""
       utf8->string ""
       string->utf8 "")
  (doc procedure? ""
       apply ""
       map ""
       for-each ""
       string-map ""
       string-for-each ""
       vector-map ""
       vector-for-each ""
       call-with-current-continuation ""
       call/cc ""
       dynamic-wind ""
       values ""
       call-with-values "")
  (doc
   with-exception-handler ""
   raise ""
   raise-continuable ""
   error ""
   error-object? ""
   error-object-message ""
   error-object-irritants ""
   read-error? ""
   file-error? "")
  (doc
   current-input-port ""
   current-output-port ""
   current-error-port ""

   call-with-port ""

   port? ""
   input-port? ""
   output-port? ""
   textual-port? ""
   binary-port? ""

   ;; (rename port-open? input-port-open?)
   ;; (rename port-open? output-port-open?)
   close-port ""
   ;; (rename close-port close-input-port)
   ;; (rename close-port close-output-port)

   open-input-string ""
   open-output-string ""
   get-output-string ""
   open-input-bytevector ""
   open-output-bytevector ""
   get-output-bytevector ""

   eof-object? ""
   eof-object ""

   read-char ""
   peek-char ""
   char-ready? ""
   read-line ""
   read-string ""

   read-u8 ""
   peek-u8 ""
   u8-ready? ""
   read-bytevector ""
   read-bytevector! ""

   newline ""
   write-char ""
   write-string ""
   write-u8 ""
   write-bytevector ""
   flush-output-port ""))
