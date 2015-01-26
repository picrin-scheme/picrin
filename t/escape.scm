(import (scheme base)
		(picrin control)
        (picrin test))

(test-begin)

(test 1 (escape (lambda (exit) (begin (exit 1) 2))))

(define cont #f)

(test "calling dead escape continuation"
	  (guard (c ((error-object? c) (error-object-message c)))
			 (escape (lambda (exit) (set! cont exit)))
			 (cont 3)))

(test-end)
