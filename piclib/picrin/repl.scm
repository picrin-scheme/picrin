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

  (define (print obj . port)
    (write obj (if (null? port) (current-output-port) (car port)))
    (newline)
    obj)

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
             (exit 1))
            ((-e)
             (cadr args))
            (else
             (file->string (car args)))))))

  (define (main-loop in out)
    (display "> " out)
    (let ((expr (read in)))
      (if (eof-object? expr)
          (newline out)                 ; exit
          (begin
            (call/cc
             (lambda (leave)
              (with-exception-handler
               (lambda (condition)
                 (display (error-object-message condition) (current-error-port))
                 (newline)
                 (leave))
               (lambda ()
                 (print (eval expr '(picrin user)) out)))))
            (main-loop in out)))))

  (define (run-repl program)
    (let ((in (if program
                  (open-input-string program)
                  (current-input-port)))
          (out (if program
                   (open-output-string) ; ignore output
                   (current-output-port))))
      (main-loop in out)))

  (define (repl)
    (let ((program (getopt)))
      (run-repl program)))

  (export repl))
