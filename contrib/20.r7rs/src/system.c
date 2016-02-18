/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"

extern int picrin_argc;
extern char **picrin_argv;
extern char **picrin_envp;

static pic_value
pic_system_cmdline(pic_state *pic)
{
  pic_value v = pic_nil_value(pic);
  int i;

  pic_get_args(pic, "");

  for (i = 0; i < picrin_argc; ++i) {
    size_t ai = pic_gc_arena_preserve(pic);

    v = pic_cons(pic, pic_obj_value(pic_cstr_value(pic, picrin_argv[i])), v);
    pic_gc_arena_restore(pic, ai);
  }

  return pic_reverse(pic, v);
}

static pic_value
pic_system_exit(pic_state *pic)
{
  pic_value v;
  int argc, status = EXIT_SUCCESS;

  argc = pic_get_args(pic, "|o", &v);
  if (argc == 1) {
    switch (pic_type(pic, v)) {
    case PIC_TYPE_FLOAT:
      status = (int)pic_float(pic, v);
      break;
    case PIC_TYPE_INT:
      status = pic_int(pic, v);
      break;
    default:
      break;
    }
  }

  pic_close(pic);

  exit(status);
}

static pic_value
pic_system_emergency_exit(pic_state *pic)
{
  pic_value v;
  int argc, status = EXIT_FAILURE;

  argc = pic_get_args(pic, "|o", &v);
  if (argc == 1) {
    switch (pic_type(pic, v)) {
    case PIC_TYPE_FLOAT:
      status = (int)pic_float(pic, v);
      break;
    case PIC_TYPE_INT:
      status = pic_int(pic, v);
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

  pic_get_args(pic, "z", &str);

  val = getenv(str);

  if (val == NULL)
    return pic_nil_value(pic);
  else
    return pic_obj_value(pic_cstr_value(pic, val));
}

static pic_value
pic_system_getenvs(pic_state *pic)
{
  char **envp;
  pic_value data = pic_nil_value(pic);
  size_t ai = pic_gc_arena_preserve(pic);

  pic_get_args(pic, "");

  if (! picrin_envp) {
    return pic_nil_value(pic);
  }

  for (envp = picrin_envp; *envp; ++envp) {
    struct pic_string *key, *val;
    int i;

    for (i = 0; (*envp)[i] != '='; ++i)
      ;

    key = pic_str_value(pic, *envp, i);
    val = pic_cstr_value(pic, getenv(pic_str(pic, key)));

    /* push */
    data = pic_acons(pic, pic_obj_value(key), pic_obj_value(val), data);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, data);
  }

  return data;
}

void
pic_init_system(pic_state *pic)
{
  pic_deflibrary(pic, "scheme.process-context");

  pic_defun(pic, "command-line", pic_system_cmdline);
  pic_defun(pic, "exit", pic_system_exit);
  pic_defun(pic, "emergency-exit", pic_system_emergency_exit);
  pic_defun(pic, "get-environment-variable", pic_system_getenv);
  pic_defun(pic, "get-environment-variables", pic_system_getenvs);
}
