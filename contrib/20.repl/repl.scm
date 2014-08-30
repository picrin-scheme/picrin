(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme file)
          (scheme write)
          (scheme eval)
          (scheme process-context))

  (define (join sep strs)
    (let loop ((result (car strs)) (rest (cdr strs)))
      (if (null? rest)
          result
          (loop (string-append result sep (car rest)) (cdr rest)))))

  (define (file->string file)
    (with-input-from-file file
      (lambda ()
        (let loop ((line (read-line)) (acc '()))
          (if (eof-object? line)
              (join "\n" (reverse acc))
              (loop (read-line) (cons line acc)))))))

  (define (print obj . port)
    (let ((port (if (null? port) (current-output-port) (car port))))
      (write obj port)
      (newline port)
      obj))

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

  (define (main-loop in out on-err)
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
                 (if on-err
                     (on-err)
                     (leave)))
               (lambda ()
                 (print (eval expr '(picrin user)) out)))))
            (main-loop in out on-err)))))

  (define (run-repl program)
    (let ((in (if program
                  (open-input-string program)
                  (current-input-port)))
          (out (if program
                   (open-output-string) ; ignore output
                   (current-output-port)))
          (on-err (if program
                      (lambda () (exit 1))
                      #f)))
      (main-loop in out on-err)))

  (define (repl)
    (let ((program (getopt)))
      (run-repl program)))

  (export repl))

