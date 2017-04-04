(import (scheme base)
        (scheme read)
        (scheme write)
        (only (picrin base) compile))

(define (generate-rom)

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
        (write (compile (read))))))

  (define (escape-string s)
    (with-output-to-string
      (lambda ()
        (string-for-each
         (lambda (c)
           (case c
             ((#\\) (write-string "\\\\"))
             ((#\") (write-string "\\\""))
             ((#\newline) (write-string "\\n"))
             (else (write-char c))))
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
   "static const char boot_rom[][80] = {\n"
   ,(generate-rom)
   "};\n"
   "\n"
   "static const char boot_compile_rom[][80] = {\n"
   ,(generate-rom)
   "};\n"
   "\n"
   "#if PIC_USE_LIBRARY\n"
   "static const char boot_library_rom[][80] = {\n"
   ,(generate-rom)
   "};\n"
   "#endif\n"
   "\n"
   "void\n"
   "pic_boot(pic_state *pic)\n"
   "{\n"
   "  pic_load_native(pic, &boot_compile_rom[0][0]);\n"
   "  pic_load_native(pic, &boot_rom[0][0]);\n"
   "#if PIC_USE_LIBRARY\n"
   "  pic_load_native(pic, &boot_library_rom[0][0]);\n"
   "#endif\n"
   "}\n"))

