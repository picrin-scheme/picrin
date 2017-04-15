(define (generate-rom)

  (define open-output-string open-output-bytevector)
  (define (get-output-string port)
    (list->string (map integer->char (bytevector->list (get-output-bytevector port)))))

  (define (with-output-to-string thunk)
    (let ((port (open-output-string)))
      (parameterize ((current-output-port port))
        (thunk)
        (let ((s (get-output-string port)))
          (close-port port)
          s))))

  (define text
    (with-output-to-string
      (lambda ()
        (write (read)))))

  (define (escape-string s)
    (with-output-to-string
      (lambda ()
        (string-for-each
         (lambda (c)
           (case c
             ((#\\) (display "\\\\"))
             ((#\") (display "\\\""))
             ((#\newline) (display "\\n"))
             (else (display c))))
         s))))

  (define (group-string i s)
    (let loop ((t s) (n (string-length s)) (acc '()))
      (if (= n 0)
          (reverse acc)
          (if (< n i)
              (loop "" 0 (cons t acc))
              (loop (string-copy t i) (- n i) (cons (string-copy t 0 i) acc))))))

  (define lines (map escape-string (group-string 80 text)))

  (let loop ((lines lines) (acc ""))
       (if (null? lines)
           acc
           (loop (cdr lines) (string-append acc "\"" (car lines) "\",\n")))))


(for-each
 display
 `("#include \"picrin.h\"\n"
   "#include \"picrin/extra.h\"\n"
   "\n"
   "#if PIC_USE_EVAL\n"
   "static const char eval_rom[][80] = {\n"
   ,(generate-rom)
   "};\n"
   "#endif\n"
   "\n"
   "void\n"
   "pic_init_eval(pic_state *PIC_UNUSED(pic))\n"
   "{\n"
   "#if PIC_USE_EVAL\n"
   "  pic_load_native(pic, &eval_rom[0][0]);\n"
   "#endif\n"
   "}\n"))

