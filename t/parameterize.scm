(import (scheme base)
        (scheme write))

; expects "piece by piece by piece.\n"
(write
 (parameterize
     ((current-output-port (open-output-string)))
   (display "piece")
   (display " by piece ")
   (display "by piece.")
   (newline)
   (get-output-string)))
