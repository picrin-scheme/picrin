(for-each
 display
 `("#include \"picrin.h\"\n"
   "#include \"picrin/extra.h\"\n"
   "\n"
   "#if PIC_USE_EVAL\n"))

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
   "pic_init_eval(pic_state *PIC_UNUSED(pic))\n"
   "{\n"
   "#if PIC_USE_EVAL\n"
   "  pic_execute(pic, pic_deserialize(pic, eval_rom));\n"
   "#endif\n"
   "}\n"))
