/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

static pic_value
var_conv(pic_state *pic, pic_value val, pic_value conv)
{
  if (! pic_false_p(pic, conv)) {
    val = pic_call(pic, conv, 1, val);
  }
  return val;
}

static pic_value
var_call(pic_state *pic)
{
  pic_value self, val;
  int n;

  n = pic_get_args(pic, "&|o", &self, &val);

  if (n == 0) {
    return pic_closure_ref(pic, 0);
  } else {

    pic_closure_set(pic, 0, var_conv(pic, val, pic_closure_ref(pic, 1)));

    return pic_undef_value(pic);
  }
}

pic_value
pic_make_var(pic_state *pic, pic_value init, pic_value conv)
{
  return pic_lambda(pic, var_call, 2, var_conv(pic, init, conv), conv);
}

static pic_value
dynamic_set(pic_state *pic)
{
  pic_value var, val;

  pic_get_args(pic, "");

  var = pic_closure_ref(pic, 0);
  val = pic_closure_ref(pic, 1);

  pic_proc_ptr(pic, var)->locals[0] = val;

  return pic_undef_value(pic);
}

pic_value
pic_dynamic_bind(pic_state *pic, pic_value var, pic_value val, pic_value thunk)
{
  pic_value in, out, new_val, old_val;

  old_val = pic_call(pic, var, 0);
  new_val = var_conv(pic, val, pic_proc_ptr(pic, var)->locals[1]);

  in = pic_lambda(pic, dynamic_set, 2, var, new_val);
  out = pic_lambda(pic, dynamic_set, 2, var, old_val);

  return pic_dynamic_wind(pic, in, thunk, out);
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  pic_value init, conv = pic_false_value(pic);

  pic_get_args(pic, "o|l", &init, &conv);

  return pic_make_var(pic, init, conv);
}

static pic_value
pic_var_dynamic_bind(pic_state *pic)
{
  pic_value var, val, thunk;

  pic_get_args(pic, "lol", &var, &val, &thunk);

  if (! (pic_proc_p(pic, var) && pic_proc_ptr(pic, var)->u.f.func == var_call)) {
    pic_error(pic, "parameter required", 1, var);
  }

  return pic_dynamic_bind(pic, var, val, thunk);
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "dynamic-bind", pic_var_dynamic_bind);
}
