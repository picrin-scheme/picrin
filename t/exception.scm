(write
 (call/cc
  (lambda (k)
    (with-exception-handler
     (lambda (x)
       (write "condition: ")
       (write x)
       (newline)
       (k 'exception))
     (lambda ()
       (+ 1 (raise 'an-error)))))))
