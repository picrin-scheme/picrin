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
        e = pic_read(pic, pic_stdin(pic));
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
        e = pic_read(pic, port);
        if (pic_eof_p(pic, e))
          break;
        pic_void(pic, pic_funcall(pic, "eval", 1, e));
      }
    } else if (argc >= 2 && strcmp(argv[1], "-c") == 0) { /* compile */
      if (argc == 2) {
        port = pic_stdin(pic);
      } else {
        FILE *file = fopen(argv[2], "r");
        if (! file) {
          fprintf(stderr, "could not open file %s\n", argv[2]);
          exit(1);
        }
        port = pic_fopen(pic, file, "r");
      }
      pic_printf(pic, "~s\n", pic_funcall(pic, "compile", 1, pic_funcall(pic, "expand", 2, pic_read(pic, port), pic_ref(pic, "default-environment"))));
    } else {
      fprintf(stderr, "usage: mini-picrin [-c] [file]\n");
      exit(1);
    }
  }
  pic_catch(e) {
    pic_print_error(pic, pic_stderr(pic), e);
  }

  pic_close(pic);
}
