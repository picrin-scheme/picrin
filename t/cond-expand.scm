(import (scheme base)
        (scheme write)
        (picrin test))

(test-begin)
(test #t (cond-expand
          (r7rs #t)
          (else #f)))

(test #t (cond-expand
          ((library '(scheme write)) #t)
          (else #f)))
(test #t (cond-expand
          ((not r6rs) #t)
          (else #f)))
(test #t (cond-expand
          ((and r7rs (library '(picrin test))) #t)
          (else #f)))

(test #t (cond-expand
          ((or r6rs r7rs) #t)
          (else #f)))
(test-end)
