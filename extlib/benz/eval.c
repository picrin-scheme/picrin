/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"
#include "picrin/private/vm.h"
#include "picrin/private/state.h"

pic_value
pic_eval(pic_state *pic, pic_value program, const char *lib)
{
  const char *prev_lib = pic_current_library(pic);
  pic_value env, r, e;

  env = pic_library_environment(pic, lib);

  pic_in_library(pic, lib);
  pic_try {
    r = pic_call(pic, pic_compile(pic, pic_expand(pic, program, env)), 0);
  }
  pic_catch(e) {
    pic_in_library(pic, prev_lib);
    pic_raise(pic, e);
  }
  pic_in_library(pic, prev_lib);

  return r;
}

static pic_value
pic_eval_eval(pic_state *pic)
{
  pic_value program;
  const char *str;

  pic_get_args(pic, "oz", &program, &str);

  return pic_eval(pic, program, str);
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
