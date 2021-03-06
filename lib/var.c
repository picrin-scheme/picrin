/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include "value.h"
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

    pic_for_each(env, pic_ref(pic, "__picrin_dynenv__"), it) {
      if (pic_attr_has(pic, env, self)) {
        return pic_attr_ref(pic, env, self);
      }
    }
    PIC_UNREACHABLE();          /* logic flaw */
  } else {
    pic_value conv;

    conv = pic_closure_ref(pic, 0);
    if (! pic_false_p(pic, conv)) {
      val = pic_call(pic, conv, 1, val);
    }
    pic_attr_set(pic, pic_car(pic, pic_ref(pic, "__picrin_dynenv__")), self, val);
    return pic_undef_value(pic);
  }
}

pic_value
pic_make_var(pic_state *pic, pic_value init, pic_value conv)
{
  pic_value var, env = pic_ref(pic, "__picrin_dynenv__");

  var = pic_lambda(pic, var_call, 1, conv);
  while (1) {
    if (pic_nil_p(pic, pic_cdr(pic, env))) { /* top dyn env */
      if (! pic_false_p(pic, conv)) {
        init = pic_call(pic, conv, 1, init);
      }
      pic_attr_set(pic, pic_car(pic, env), var, init);
      break;
    }
    env = pic_cdr(pic, env);
  }
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
pic_var_current_dynamic_environment(pic_state *pic)
{
  pic_value dyn_env;
  int n;

  n = pic_get_args(pic, "|o", &dyn_env);

  if (n == 0) {
    return pic_ref(pic, "__picrin_dynenv__");
  } else {
    pic_set(pic, "__picrin_dynenv__", dyn_env);
    return pic_undef_value(pic);
  }
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "current-dynamic-environment", pic_var_current_dynamic_environment);
}
