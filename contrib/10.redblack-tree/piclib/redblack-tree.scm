(define-library (picrin rb-tree)
  (import (scheme base)
          (scheme write))
  (begin
    (define-record-type node
      (make-node color left key right)
      node?
      (color node-color)                     ; 'black or 'red
      (left node-left set-left!)             ; node or 'leaf
      (key node-key)                         ; `compare'able
      (right node-right set-right!)          ; node or 'leaf
      )

    (define-record-type rb-tree
      (%make-rb-tree cmp)
      rb-tree?
      (node rb-tree-node set-node!)
      (cmp rb-tree-compare)
      (size rb-tree-size set-size!))
    
    (define (compare i1 i2)
      (- i1 i2))

    (define (red left key right)
      (make-node 'red left key right))
    (define (red? node)
      (and (node? node) (eqv? (node-color node) 'red)))

    (define (black left key right)
      (make-node 'black left key right))
    (define (black? node)
      (and (node? node) (eqv? (node-color node) 'black))))
  
  (begin
    (define-syntax expand-node-match?
      (syntax-rules (red black leaf)
        ((_ node (red left key right))
         (and (red? node)
              (expand-node-match? (node-left node) left)
              (expand-node-match? (node-right node) right)))
        ((_ node (black left key right))
         (and (black? node)
              (expand-node-match? (node-left node)  left)
              (expand-node-match? (node-right node) right)))
        ((_ node (color left key right))
         (and (node? node)
              (expand-node-match? (node-left node)  left)
              (expand-node-match? (node-right node) right)))
        ((_ node leaf)
         (begin (display "called") (eqv? node 'leaf)))
        ((_ node sym)
         #t)))

    (define-syntax node-bind-unsafe
      (syntax-rules (red black leaf)
        ((_ (red left key right) node expr)
         (let ((key (node-key node)))
           (node-bind-unsafe left (node-left node)
                             (node-bind-unsafe right (node-right node)
                                               expr))))
        ((_ (black left key right) node expr)
         (let ((key (node-key node)))
           (node-bind-unsafe left (node-left node)
                             (node-bind-unsafe right (node-right node)
                                               expr))))
        ((_ (color left key right) node expr)
         (let ((key (node-key node))
               (color (node-color node)))
           (node-bind-unsafe left (node-left node)
                             (node-bind-unsafe right (node-right node)
                                               expr))))
        ((_ leaf node expr)
         expr)
        ((_ sym node expr)
         (let ((sym node))
           expr))))

    (define-syntax expand-or-pattern
      (syntax-rules ()
        ((_ node () expr else)
         else)
        ((_ node (pattern r ...) expr else)
         (expand-pattern node pattern expr
             (expand-or-pattern node (r ...) expr else)))))

    (define-syntax expand-pattern
      (syntax-rules (or)
        ((_ node (or pattern ...)  expr else)
         (expand-or-pattern node (pattern ...) expr else))
        ((_ node pattern expr else)
         (if (expand-node-match? node pattern)
             (node-bind-unsafe pattern node expr)
             else))))

    (define-syntax node-match
      (syntax-rules (else)
        ((_ node (else expr ...))
         (begin expr ...))
        ((_ node (pattern expr ...))
         (let ((node% node))
          (expand-pattern node% pattern
                          (begin expr ...)
                          #f)))
        ((_ node (pattern expr ...) (rest-pattern rest-expr ...) ...)
         (let ((node% node))
          (expand-pattern node% pattern
                          (begin expr ...)
                          (node-match node% (rest-pattern rest-expr ...) ...))))))
    

    (define (make-rb-tree . cmp)
      (let* ((cmp (if (null? cmp) compare cmp))
             (rb (%make-rb-tree cmp)))
        (set-node! rb 'leaf)
        (set-size! rb 0)
        rb))

    (define (balance node)
      (node-match node
                  ((or (black (red (red a x b) y c) z d)
                       (black (red a x (red b y c)) z d)
                       (black a x (red (red b y c) z d))
                       (black a x (red b y (red c z d))))
                   (red (black a x b) y (black c z d)))
                  (else node)))
    
    (define (rb-tree-insert rb-tree key)
      (let ((node (rb-tree-node rb-tree))
            (cmp (rb-tree-compare rb-tree))
            (inserted #t))
        (letrec ((ins (lambda (node)
                        (node-match node
                                    ((color left label right)
                                     (cond ((< (cmp key label) 0)
                                            (balance (make-node color (ins left) label right)))
                                           ((> (cmp key label) 0)
                                            (balance (make-node color left label (ins right))))
                                           (#t (set! inserted #f) node)))
                                    (leaf (red 'leaf key 'leaf))))))

          (set-node! rb-tree
           (node-match (ins node)
                       ((node left label right)
                        (black left label right))))
          (if inserted
              (set-size! rb-tree (+ (rb-tree-size rb-tree) 1)))
          inserted)))
    
    (define (node-mem? node cmp key)
      (if (eqv? node 'leaf)
          #f
          (let ((ord (cmp (node-key node) key)))
            (cond
             ((< ord 0) (node-mem? (node-right node) cmp key))
             ((= ord 0) #t)
             ((> ord 0) (node-mem? (node-left node) cmp key))))))

    (define (rb-tree-mem? rb-tree key)
      (node-mem? (rb-tree-node rb-tree) (rb-tree-compare rb-tree) key))

    (define (node-display node last?)
      (node-match node
                  ((c left key leaf)
                   (node-display left #f)
                   (display key)
                   (display "[debug ") (display leaf) (display "]")
                   (if (not last?) (display " ")))                  
                  ((c left key right)
                   (node-display left #f)
                   (display key)
                   (node-display right last?))))
    (define (rb-tree-display rb-tree)
      (display "(rb-tree ")
      (node-display (rb-tree-node rb-tree) #t)
      (display ")"))

    (define (rb-tree-delete rb-tree key)))
  (export make-rb-tree
          rb-tree-size
          rb-tree-insert
          rb-tree-mem?
          rb-tree-display))
