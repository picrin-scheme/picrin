/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

void
pic_init_picrin(pic_state *pic)
{
  void pic_init_contrib(pic_state *);
  void pic_load_piclib(pic_state *);

  pic_init_contrib(pic);
  pic_load_piclib(pic);
}

int picrin_argc;
char **picrin_argv;
char **picrin_envp;

extern char *picrin_native_stack_start; /* for call/cc */

int
main(int argc, char *argv[], char **envp)
{
  char t;
  pic_state *pic;
  int status;

  pic = pic_open(pic_default_allocf, NULL);

  picrin_argc = argc;
  picrin_argv = argv;
  picrin_envp = envp;

  picrin_native_stack_start = &t;

  pic_try {
    pic_init_picrin(pic);

    pic_funcall(pic, "picrin.main", "main", 0);

    status = 0;
  }
  pic_catch {
    pic_print_error(pic, xstderr);
    status = 1;
  }

  pic_close(pic);

  return status;
}
