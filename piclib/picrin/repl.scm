(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme file)
          (scheme write)
          (scheme eval)
          (scheme process-context))

  (define (file->string file)
    (with-input-from-file file
      (lambda ()
        (let loop ((line (read-line)))
          (if (eof-object? line)
              ""
              (string-append line (loop (read-line))))))))

  (define (print-help)
    (display "picrin scheme\n")
    (display "\n")
    (display "Usage: picrin [options] [file]\n")
    (display "\n")
    (display "Options:\n")
    (display "  -e [program]		run one liner script\n")
    (display "  -h or --help		show this help\n"))

  (define (getopt)
    (let ((args (cdr (command-line))))
      (if (null? args)
          #f
          (case (string->symbol (car args))
            ((-h --help)
             (print-help)
             (exit 0))
            ((-e)
             (cadr args))
            (else
             (file->string (car args)))))))

  (define (print obj)
    (write obj)
    (newline))

  (define (main-loop)
    (display "> ")
    (let ((expr (read)))
      (if (eof-object? expr)
          (newline)                     ; exit
          (begin
            (call/cc
             (lambda (leave)
              (with-exception-handler
               (lambda (condition)
                 (display (error-object-message condition))
                 (newline)
                 (leave))
               (lambda ()
                 (print (eval expr '(picrin user)))))))
            (main-loop)))))

  (define (repl)
    (let ((program (getopt)))
      (parameterize
          ((current-input-port
            (if program
                (open-input-string program)
                (current-input-port))))
        (main-loop))))

  (export repl))
