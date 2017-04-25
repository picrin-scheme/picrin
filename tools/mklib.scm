(for-each
 display
 `("#include \"picrin.h\"\n"
   "#include \"picrin/extra.h\"\n"
   "\n"
   "static "))

(let loop ()
  (let ((c (read-u8)))
    (unless (eof-object? c)
      (write-u8 c)
      (loop))))

(for-each
 display
 `("\n"
   "void\n"
   "pic_init_lib(pic_state *PIC_UNUSED(pic))\n"
   "{\n"
   "  pic_call(pic, pic_deserialize(pic, pic_blob_value(pic, lib_rom, sizeof lib_rom)), 0);\n"
   "}\n"))
