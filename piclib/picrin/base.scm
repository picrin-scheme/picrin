(define-library (picrin base)
  (export define
          set!
          quote
          lambda
          if
          begin
          define-syntax)

  (export bytevector?
          make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-copy!
          bytevector-append)

  (export eq?
          eqv?
          equal?)

  (export boolean?
          boolean=?
          not)

  (export char?
          char->integer
          integer->char)

  (export call-with-current-continuation
          continue
          dynamic-wind
          values
          call-with-values)

  (export make-dictionary
          dictionary?
          dictionary-ref
          dictionary-set!
          dictionary-delete
          dictionary-size
          dictionary-for-each)

  (export with-exception-handler
          raise
          raise-continuable
          error
          error-object?
          error-object-message
          error-object-irritants
          read-error?
          file-error?)

  (export identifier?
          identifier=?
          make-identifier)

  (export number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          exact-integer?
          =
          <
          >
          <=
          >=
          zero?
          positive?
          negative?
          odd?
          even?
          min
          max
          +
          -
          *
          /
          abs
          floor-quotient
          floor-remainder
          floor/
          truncate-quotient
          truncate-remainder
          truncate/
          gcd
          lcm
          floor
          ceiling
          truncate
          round
          exact-integer-sqrt
          square
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

  (export current-input-port
          current-output-port
          current-error-port
          input-port?
          output-port?
          textual-port?
          binary-port?
          port?
          input-port-open?
          output-port-open?
          close-port
          close-input-port
          close-output-port

          open-input-string
          open-output-string
          get-output-string
          open-input-bytevector
          open-output-bytevector
          get-output-bytevector

          read-char
          peek-char
          read-line
          eof-object?
          eof-object
          char-ready?
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

  (export procedure?
          apply
          map
          for-each
          attribute)

  (export read)

  (export make-record
          record?
          record-type
          record-ref
          record-set!)

  (export string?
          make-string
          string-length
          string-ref
          string-set!
          string=?
          string<?
          string>?
          string<=?
          string>=?
          string-copy
          string-copy!
          string-append
          string-fill!)

  (export symbol?
          symbol->string
          string->symbol
          symbol=?)

  (export make-parameter
          parameter-ref
          parameter-set!
          parameter-push!
          parameter-pop!)

  (export vector?
          make-vector
          vector-length
          vector-ref
          vector-set!
          vector-copy!
          vector-copy
          vector-append
          vector-fill!
          list->vector
          vector->list)

  (export write
          write-simple
          write-shared
          display))
