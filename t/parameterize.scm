(import (scheme base)
        (scheme write)
        (picrin test))

(test-begin)

(test "piece by piece by piece.\n"
      (parameterize
          ((current-output-port (open-output-string)))
        (display "piece")
        (display " by piece ")
        (display "by piece.")
        (newline)
        (get-output-string)))

(test-end)
