(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval)
          (picrin base))

  (cond-expand
   ((library (picrin readline))
    (import (picrin readline)
            (picrin readline history)))
   (else
    (begin
      (define (readline str)
        (when (tty?)
          (display str)
          (flush-output-port))
        (read-line))
      (define (add-history str)
        #f))))

  (define (init-env)
    (eval
     '(import (scheme base)
              (scheme load)
              (scheme process-context)
              (scheme read)
              (scheme write)
              (scheme file)
              (scheme inexact)
              (scheme cxr)
              (scheme lazy)
              (scheme time)
              (scheme eval)
              (scheme r5rs)
              (picrin macro))
     '(picrin user)))

  (define (repeat x)
    (let ((p (list x)))
      (set-cdr! p p)
      p))

  (define (join xs delim)
    (cdr (apply append (map list (repeat delim) xs))))

  (define (string-join strings delim)
    (apply string-append (join strings delim)))

  (define (->string x)
    (call-with-port (open-output-string)
      (lambda (port)
        (write x port)
        (get-output-string port))))

  (define (print-error-object e)
    (define type (error-object-type e))
    (unless (eq? type '||)
      (display type)
      (display "-"))
    (display "error: ")
    (display (error-object-message e))
    (display ".")
    (define irritants (error-object-irritants e))
    (unless (null? irritants)
      (display " (irritants: ")
      (display (string-join (map ->string irritants) ", "))
      (display ")"))
    (newline))

  (define (repl)
    (init-env)
    (let loop ((buf ""))
      (let ((line (readline (if (equal? buf "") "> " ""))))
        (if (eof-object? line)
            (newline)                   ; exit
            (let ((str (string-append buf line "\n")))
              (add-history line)
              (call/cc
               (lambda (exit)
                 (with-exception-handler
                  (lambda (condition)
                    (if (error-object? condition)
                        (unless (equal? (error-object-message condition) "unexpected EOF")
                          (print-error-object condition)
                          (set! str ""))
                        (begin
                          (display "raise: ")
                          (write condition)
                          (newline)
                          (set! str "")))
                    (exit))
                  (lambda ()
                    (call-with-port (open-input-string str)
                      (lambda (port)
                        (let next ((expr (read port)))
                          (unless (eof-object? expr)
                            (write (eval expr))
                            (newline)
                            (set! str "")
                            (next (read port))))))))))
              (loop str))))))

  (export repl))

