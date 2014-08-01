/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/error.h"

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  int status = 0;

  pic = pic_open(argc, argv, envp);

  pic_try {
    pic_import(pic, pic_read_cstr(pic, "(picrin repl)"));

    pic_funcall(pic, "repl", pic_nil_value());

    pic_run(pic);
  }
  pic_catch {
    pic_print_backtrace(pic, pic->err);
    status = 1;
  }

  pic_close(pic);

  return status;
}
