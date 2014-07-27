(define-library (foo)
  (import (except (rename (prefix (only (scheme base) car cdr cons) my-)
                          (my-car my-kar)
                          (my-cdr my-kdr))
                  my-kar))

  ;; (import (rename (scheme base)
  ;;                 (car my-kar)
  ;;                 (cdr my-cdr)))

  (export my-kdr my-cons))
