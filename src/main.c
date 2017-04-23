/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

void
pic_init_picrin(pic_state *pic)
{
  void pic_init_lib(pic_state *);
  void pic_init_contrib(pic_state *);
  void pic_load_piclib(pic_state *);

  pic_init_lib(pic);
  pic_init_contrib(pic);
  pic_load_piclib(pic);
}

int picrin_argc;
char **picrin_argv;
char **picrin_envp;

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  pic_value e;
  int status;

  pic = pic_open(pic_default_allocf, NULL);

  picrin_argc = argc;
  picrin_argv = argv;
  picrin_envp = envp;

  pic_try {
    pic_init_picrin(pic);

    pic_funcall(pic, "picrin.main:main", 0);

    status = 0;
  }
  pic_catch(e) {
    pic_funcall(pic, "display", 2, e, pic_stderr(pic));
    status = 1;
  }

  pic_close(pic);

  return status;
}
