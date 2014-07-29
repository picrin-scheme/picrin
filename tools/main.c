/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/error.h"

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;

  pic = pic_open(argc, argv, envp);

  pic_try {
    pic_import(pic, pic_read_cstr(pic, "(picrin repl)"));
    pic_funcall(pic, "repl", pic_nil_value());
  }
  pic_catch {
    pic_print_backtrace(pic, pic->err);
  }

  pic_close(pic);

  return 0;
}
