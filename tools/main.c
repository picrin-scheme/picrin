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
    pic_load(pic, "/Users/yuichi/workspace/picrin/tools/main.scm");
  }
  pic_catch {
    pic_print_backtrace(pic, pic->err);
  }

  pic_close(pic);

  return 0;
}
