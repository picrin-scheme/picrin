/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

/* implementated by deep binding */

static pic_value
var_call(pic_state *pic)
{
  pic_value self, val;
  int n;

  n = pic_get_args(pic, "&|o", &self, &val);

  if (n == 0) {
    pic_value env, it;

    pic_for_each(env, pic->dyn_env, it) {
      if (pic_weak_has(pic, env, self)) {
        return pic_weak_ref(pic, env, self);
      }
    }
    PIC_UNREACHABLE();          /* logic flaw */
  } else {
    pic_value conv;

    conv = pic_closure_ref(pic, 0);
    if (! pic_false_p(pic, conv)) {
      val = pic_call(pic, conv, 1, val);
    }
    pic_weak_set(pic, pic_car(pic, pic->dyn_env), self, val);
    return pic_undef_value(pic);
  }
}

pic_value
pic_make_var(pic_state *pic, pic_value init, pic_value conv)
{
  pic_value var;

  var = pic_lambda(pic, var_call, 1, conv);
  pic_call(pic, var, 1, init);
  return var;
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  pic_value init, conv = pic_false_value(pic);

  pic_get_args(pic, "o|l", &init, &conv);

  return pic_make_var(pic, init, conv);
}

static pic_value
pic_var_with_dynamic_environment(pic_state *pic)
{
  pic_value alist, thunk, env, it, elt, val;

  pic_get_args(pic, "ol", &alist, &thunk);

  env = pic_make_weak(pic);
  pic_for_each(elt, alist, it) {
    pic_weak_set(pic, env, pic_car(pic, elt), pic_cdr(pic, elt));
  }
  pic->dyn_env = pic_cons(pic, env, pic->dyn_env);
  val = pic_call(pic, thunk, 0);
  pic->dyn_env = pic_cdr(pic, pic->dyn_env);
  return val;
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "with-dynamic-environment", pic_var_with_dynamic_environment);
}
