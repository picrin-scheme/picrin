(define-library (picrin dictionary)
  (import (picrin base))

  (export dictionary?
          dictionary
          make-dictionary
          dictionary-ref
          dictionary-set!
          dictionary-delete
          dictionary-size
          dictionary->plist
          plist->dictionary
          dictionary->alist
          alist->dictionary))
