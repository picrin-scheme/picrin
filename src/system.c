#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"

static pic_value
pic_system_cmdline(pic_state *pic)
{
  pic_value v = pic_nil_value();
  int i;

  pic_get_args(pic, "");

  for (i = 0; i < pic->argc; ++i) {
    int ai = pic_gc_arena_preserve(pic);

    v = pic_cons(pic, pic_str_new_cstr(pic, pic->argv[i]), v);
    pic_gc_arena_restore(pic, ai);
  }

  return v;
}

static pic_value
pic_system_exit(pic_state *pic)
{
  pic_value v;
  int argc, status = EXIT_SUCCESS;

  argc = pic_get_args(pic, "|o", &v);
  if (argc == 1) {
    switch (pic_type(v)) {
    case PIC_TT_FLOAT:
      status = (int)pic_float(v);
      break;
    case PIC_TT_INT:
      status = pic_int(v);
      break;
    default:
      break;
    }
  }

  exit(status);
}

static pic_value
pic_system_emergency_exit(pic_state *pic)
{
  pic_value v;
  int argc, status = EXIT_FAILURE;

  argc = pic_get_args(pic, "|o", &v);
  if (argc == 1) {
    switch (pic_type(v)) {
    case PIC_TT_FLOAT:
      status = (int)pic_float(v);
      break;
    case PIC_TT_INT:
      status = pic_int(v);
      break;
    default:
      break;
    }
  }

  _Exit(status);
}

static pic_value
pic_system_getenv(pic_state *pic)
{
  char *str, *val;
  size_t len;

  pic_get_args(pic, "s", &str, &len);

  val = getenv(str);

  if (val == NULL)
    return pic_nil_value();
  else
    return pic_str_new_cstr(pic, val);
}

static pic_value
pic_system_getenvs(pic_state *pic)
{
  char **envp;
  pic_value data = pic_nil_value();
  int ai = pic_gc_arena_preserve(pic);

  pic_get_args(pic, "");

  for (envp = pic->envp; *envp; ++envp) {
    pic_value key, val;
    int i;

    for (i = 0; (*envp)[i] != '='; ++i)
      ;

    key = pic_str_new(pic, *envp, i);
    val = pic_str_new_cstr(pic, getenv(*envp));

    /* push */
    data = pic_acons(pic, key, val, data);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, data);
  }

  return data;
}

void
pic_init_system(pic_state *pic)
{
  pic_defun(pic, "command-line", pic_system_cmdline);
  pic_defun(pic, "exit", pic_system_exit);
  pic_defun(pic, "emergency-exit", pic_system_emergency_exit);
  pic_defun(pic, "get-environment-variable", pic_system_getenv);
  pic_defun(pic, "get-environment-variables", pic_system_getenvs);
}
