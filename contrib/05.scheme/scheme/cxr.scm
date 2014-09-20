;;; Appendix A. Standard Libraries CxR

(define-library (scheme cxr)
  (import (scheme base))

  (define (caaar p) (car (caar p)))
  (define (caadr p) (car (cadr p)))
  (define (cadar p) (car (cdar p)))
  (define (caddr p) (car (cddr p)))
  (define (cdaar p) (cdr (caar p)))
  (define (cdadr p) (cdr (cadr p)))
  (define (cddar p) (cdr (cdar p)))
  (define (cdddr p) (cdr (cddr p)))
  (define (caaaar p) (caar (caar p)))
  (define (caaadr p) (caar (cadr p)))
  (define (caadar p) (caar (cdar p)))
  (define (caaddr p) (caar (cddr p)))
  (define (cadaar p) (cadr (caar p)))
  (define (cadadr p) (cadr (cadr p)))
  (define (caddar p) (cadr (cdar p)))
  (define (cadddr p) (cadr (cddr p)))
  (define (cdaaar p) (cdar (caar p)))
  (define (cdaadr p) (cdar (cadr p)))
  (define (cdadar p) (cdar (cdar p)))
  (define (cdaddr p) (cdar (cddr p)))
  (define (cddaar p) (cddr (caar p)))
  (define (cddadr p) (cddr (cadr p)))
  (define (cdddar p) (cddr (cdar p)))
  (define (cddddr p) (cddr (cddr p)))

  (export caaar caadr cadar caddr
          cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr
          cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr
          cddaar cddadr cdddar cddddr))
