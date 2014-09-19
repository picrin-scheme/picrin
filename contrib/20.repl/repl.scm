(define-library (picrin repl)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme eval))

  (cond-expand
   ((library (picrin readline))
    (import (picrin readline)
            (picrin readline history)))
   (else
    (begin
      (define (readline str)
        (display str)
        (read-line))
      (define (add-history str)
        #f))))

  (define (repl)
    (let loop ((buf ""))
      (let ((line (readline (if (equal? buf "") "> " "* "))))
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
                          (display "error: ")
                          (display (error-object-message condition))
                          (newline)
                          (set! str ""))
                        (begin
                          (display "raised: ")
                          (write condition)
                          (newline)
                          (set! str "")))
                    (exit))
                  (lambda ()
                    (call-with-port (open-input-string str)
                      (lambda (port)
                        (let next ((expr (read port)))
                          (unless (eof-object? expr)
                            (write (eval expr '(picrin user)))
                            (newline)
                            (set! str "")
                            (next (read port))))))))))
              (loop str))))))

  (export repl))

