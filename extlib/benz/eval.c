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
  pic_value program, env;

  pic_get_args(pic, "oo", &program, &env);

  pic_assert_type(pic, env, env);

  return pic_eval(pic, program, pic_env_ptr(env));
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
