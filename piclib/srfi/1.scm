(define-library (srfi 1)
  (import (scheme base))

  ;; # Constructors
  ;; cons list
  ;; xcons cons* make-list list-tabulate
  ;; list-copy circular-list iota

  ;; # Predicates
  ;; pair? null?
  ;; proper-list? cirtular-list? dotted-list?
  ;; not-pair? null-list?
  ;; list=

  ;; # Selectors
  ;; car cdr ... cddadr cddddr list-ref
  ;; first second third fourth fifth sixth seventh eighth ninth tenth
  ;; car+cdr
  ;; take drop
  ;; take-right drop-right
  ;; take! drop-right!
  ;; split-at split-at!
  ;; last last-pair

  ;; # Miscellaneous
  ;; length length+
  ;; append concatenate reverse
  ;; append! concatenate! reverse!
  ;; append-reverse append-reverse!
  ;; zip unzip1 unzip2 unzip3 unzip4 unzip5
  ;; count

  ;; # Fold, unfold & map
  ;; map for-each
  ;; fold unfold pair-fold reduce
  ;; fold-right unfold-right pair-fold right reduce-right
  ;; append-map append-map!
  ;; map! pair-for-each filter-map map-in-order

  ;; # Filtering & partitioning
  ;; filter partition remove
  ;; filter! partition! remove!

  ;; # Searching
  ;; member memq memv
  ;; find find-tail
  ;; any every
  ;; list-index
  ;; take-while drop-while take-while!
  ;; span break span! break!

  ;; # Deleting
  ;; delete delete-duplicates
  ;; delete! delete-duplicates!

  ;; # Association lists
  ;; assoc assq assv
  ;; alist-cons alist-copy
  ;; alist-delete alist-delete!

  ;; # Set operations on lists
  ;; lset<= lset= lset-adjoin
  ;; lset-union lset-union!
  ;; lset-intersection lset-intersection!
  ;; lset-difference lset-difference!
  ;; lset-xor lset-xor!
  ;; lset-diff+intersenction lset-diff+intersection!

  ;; # Primitive side-effects
  ;; set-car! set-cdr!
  )
