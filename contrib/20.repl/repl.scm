(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval)
          (picrin macro)
          (picrin library))

  (define-syntax define-readline
    (er-macro-transformer
     (lambda (form rename compare)
       (if (member '(picrin readline) (libraries))
           `(import (picrin readline)
                    (picrin readline history))
           `(begin
              (define (readline str)
                (display str)
                (read-line))
              (define (add-history str)
                #f))))))

  (define-readline)

  (define (repl)
    (let ((line (readline "> ")))
      (if (eof-object? line)
          (newline)                     ; exit
          (begin
            (add-history line)
            (call/cc
             (lambda (exit)
              (with-exception-handler
               (lambda (condition)
                 (display (error-object-message condition) (current-error-port))
                 (newline)
                 (exit))
               (lambda ()
                 (let ((port (open-input-string line)))
                   (let loop ((expr (read port)))
                     (unless (eof-object? expr)
                       (write (eval expr '(picrin user)))
                       (newline)
                       (loop (read port))))
                   (close-port port))))))
            (repl)))))

  (export repl))

