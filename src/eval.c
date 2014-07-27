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

  pic_get_args(pic, "oo", &program, &spec);

  return pic_eval(pic, program, pic_find_library(pic, spec));
}

void
pic_init_eval(pic_state *pic)
{
  pic_deflibrary ("(scheme eval)") {
    pic_defun(pic, "eval", pic_eval_eval);
  }
}
