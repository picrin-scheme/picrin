(let ((port (open-input-file "piclib/error.c")))
  (let loop ()
    (let ((c (read-u8 port)))
      (unless (eof-object? c)
        (write-u8 c)
        (loop)))))

(for-each
 display
 `("\n"
   "#if PIC_USE_ERROR\n"
   "static "))

(let loop ()
  (let ((c (read-u8)))
    (unless (eof-object? c)
      (write-u8 c)
      (loop))))

(for-each
 display
 `("#endif\n"
   "\n"
   "void\n"
   "pic_init_error(pic_state *PIC_UNUSED(pic))\n"
   "{\n"
   "#if PIC_USE_ERROR\n"
   "  pic_call(pic, pic_deserialize(pic, pic_blob_value(pic, error_rom, sizeof error_rom)), 0);\n"
   "#endif\n"
   "}\n"))
