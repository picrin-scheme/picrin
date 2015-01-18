(import (scheme base)
        (scheme write)
        (picrin write color))

(for-each
           (lambda (color)
             (call-with-colored-output color
                                       (lambda ()
                                         (display "this is ")
                                         (display color)
                                         (newline))))
           '(black red green yellow blue magenta cyan white))

