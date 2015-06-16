/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

pic_value
pic_eval(pic_state *pic, pic_value program, struct pic_env *env)
{
  struct pic_proc *proc;

  proc = pic_compile(pic, program, env);

  return pic_apply0(pic, proc);
}

static pic_value
pic_eval_eval(pic_state *pic)
{
  pic_value program, spec;
  struct pic_lib *lib;

  pic_get_args(pic, "oo", &program, &spec);

  lib = pic_find_library(pic, spec);
  if (lib == NULL) {
    pic_errorf(pic, "no library found: ~s", spec);
  }
  return pic_eval(pic, program, lib->env);
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
