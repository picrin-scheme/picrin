#!/bin/sh

$PICRIN <<'EOF'
(import (srfi 1))
(import (picrin repl))
(every = '(1 2 3) '(1 2 3))

EOF
