(define-library (picrin base)

  (export define
          lambda
          if
          quote
          set!
          begin
          define-syntax)

  (export syntax-error
          let-syntax
          letrec-syntax)

  (export let
          let*
          letrec
          letrec*
          quasiquote
          unquote
          unquote-splicing
          and
          or
          cond
          case
          =>
          else
          do
          when
          unless)

  (export let-values
          let*-values
          define-values)

  (export eq?
          eqv?
          equal?)

  (export boolean?
          boolean=?
          not)

  (export symbol?
          symbol->string
          string->symbol
          symbol=?)

  (export char?
          char->integer
          integer->char
          char=?
          char<?
          char>?
          char<=?
          char>=?)

  (export number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          =
          <
          >
          <=
          >=
          +
          -
          *
          /
          abs
          floor/
          truncate/
          floor
          ceiling
          truncate
          round
          expt
          number->string
          string->number
          finite?
          infinite?
          nan?
          exp
          log
          sin
          cos
          tan
          acos
          asin
          atan
          sqrt)

  (export pair?
          cons
          car
          cdr
          set-car!
          set-cdr!
          null?
          caar
          cadr
          cdar
          cddr)

  (export list?
          make-list
          list
          length
          append
          reverse
          list-tail
          list-ref
          list-set!
          list-copy
          memq
          memv
          member
          assq
          assv
          assoc)

  (export bytevector?
          bytevector
          make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-copy
          bytevector-copy!
          bytevector-append
          bytevector->list
          list->bytevector)

  (export vector?
          vector
          make-vector
          vector-length
          vector-ref
          vector-set!
          vector-copy!
          vector-copy
          vector-append
          vector-fill!
          list->vector
          vector->list
          string->vector
          vector->string)

  (export string?
          string
          make-string
          string-length
          string-ref
          string-set!
          string-copy
          string-copy!
          string-append
          string-fill!
          string->list
          list->string
          string=?
          string<?
          string>?
          string<=?
          string>=?)

  (export make-dictionary
          dictionary?
          dictionary-ref
          dictionary-set!
          dictionary-delete
          dictionary-size)

  (export make-record
          record?
          record-type
          record-ref
          record-set!)

  (export current-input-port
          current-output-port
          current-error-port

          call-with-port

          port?
          input-port?
          output-port?
          textual-port?
          binary-port?

          port-open?
          close-port

          open-input-file
          open-output-file
          open-binary-input-file
          open-binary-output-file
          open-input-string
          open-output-string
          get-output-string
          open-input-bytevector
          open-output-bytevector
          get-output-bytevector

          eof-object?
          eof-object

          read-char
          peek-char
          char-ready?
          read-line
          read-string

          read-u8
          peek-u8
          u8-ready?
          read-bytevector
          read-bytevector!

          newline
          write-char
          write-string
          write-u8
          write-bytevector
          flush-output-port)

  (export make-parameter
          parameter-ref
          parameter-set!
          parameter-push!
          parameter-pop!)

  (export identifier?
          identifier=?
          make-identifier)

  (export call-with-current-continuation
          call/cc
          dynamic-wind
          values
          call-with-values)

  (export with-exception-handler
          raise
          raise-continuable
          error
          error-object?
          error-object-message
          error-object-irritants
          read-error?
          file-error?)

  (export procedure?
          apply
          map
          for-each
          attribute)

  (export read)

  (export write
          write-simple
          write-shared
          display)

  (export command-line
          exit
          emergency-exit
          file-exists?
          delete-file
          get-environment-variable
          get-environment-variables)

  (export current-second
          current-jiffy
          jiffies-per-second)

  (export eval)

  (export load))
