(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval))

  (define (repl)
    (display "> ")
    (let ((expr (read)))
      (if (eof-object? expr)
          (newline)                     ; exit
          (begin
            (call/cc
             (lambda (exit)
              (with-exception-handler
               (lambda (condition)
                 (display (error-object-message condition) (current-error-port))
                 (newline)
                 (exit))
               (lambda ()
                 (write (eval expr '(picrin user)))
                 (newline)))))
            (repl)))))

  (export repl))

