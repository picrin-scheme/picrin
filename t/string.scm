(import (scheme base)
        (scheme write))

;; master returns shorter string

(let ((str (make-string 5 #\-)))
  (string-set! str 2 #\x))

;; this code prints right result
(let ((str (make-string 5 #\-)))
  (string-fill! str #\x)
  (write str) (newline)
  (string-fill! str #\y 1)
  (write str) (newline)
  (string-fill! str #\z 2 3)
  (write str)) (newline)

;; the last one prints shorter string
(let ((str (make-string 5 #\-)))
  (string-fill! str #\x)
  (string-fill! str #\y 1)
  (string-fill! str #\z 2 3)
  (write str))

