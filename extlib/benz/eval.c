/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/macro.h"

pic_value
pic_eval(pic_state *pic, pic_value program, struct pic_lib *lib)
{
  struct pic_proc *proc;

  proc = pic_compile(pic, program, lib);

  return pic_apply(pic, proc, pic_nil_value());
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
  return pic_eval(pic, program, lib);
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
