/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

int
main(int argc, char *argv[])
{
  pic_state *pic;
  pic_value e, port;

  pic = pic_open(pic_default_allocf, NULL);

  pic_try {
    if (argc == 1) {            /* repl */
      while (1) {
        pic_printf(pic, "> ");
        e = pic_funcall(pic, "read", 0);
        if (pic_eof_p(pic, e))
          break;
        pic_printf(pic, "~s\n", pic_funcall(pic, "eval", 1, e));
      }
    } else if (argc == 2) {     /* load file */
      FILE *file = fopen(argv[1], "r");
      if (! file) {
        fprintf(stderr, "could not open file %s\n", argv[1]);
        exit(1);
      }
      port = pic_fopen(pic, file, "r");
      while (1) {
        e = pic_funcall(pic, "read", 1, port);
        if (pic_eof_p(pic, e))
          break;
        pic_void(pic, pic_funcall(pic, "eval", 1, e));
      }
    } else if (argc >= 3 && strcmp(argv[1], "-c") == 0) { /* compile */
      const char *name = argv[2];
      const unsigned char *bin;
      int len, i;
      if (argc == 3) {
        port = pic_stdin(pic);
      } else {
        FILE *file = fopen(argv[3], "r");
        if (! file) {
          fprintf(stderr, "could not open file %s\n", argv[3]);
          exit(1);
        }
        port = pic_fopen(pic, file, "r");
      }
      bin = pic_blob(pic, pic_serialize(pic, pic_funcall(pic, "compile", 1, pic_funcall(pic, "read", 1, port))), &len);
      printf("const unsigned char %s[] = {\n", name);
      for (i = 0; i < len; ++i) {
        printf("0x%02x,", bin[i]);
        if ((i + 1) % 12 == 0) {
          putchar('\n');
        } else {
          putchar(' ');
        }
      }
      if (len != 0) {
        puts("");
      }
      printf("};\n");
    } else {
      fprintf(stderr, "usage: mini-picrin [-c] [file]\n");
      exit(1);
    }
  }
  pic_catch(e) {
    pic_funcall(pic, "display", 2, e, pic_stderr(pic));
  }

  pic_close(pic);
}
