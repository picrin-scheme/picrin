/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

static pic_value
var_get(pic_state *pic, pic_value var)
{
  pic_value weak, it;

  pic_for_each (weak, pic->ptable, it) {
    if (pic_weak_has(pic, weak, var)) {
      return pic_weak_ref(pic, weak, var);
    }
  }
  pic_panic(pic, "logic flaw");
}

static pic_value
var_set(pic_state *pic, pic_value var, pic_value val)
{
  pic_value weak;

  weak = pic_car(pic, pic->ptable);

  pic_weak_set(pic, weak, var, val);

  return pic_undef_value(pic);
}

static pic_value
var_call(pic_state *pic)
{
  pic_value self, val;
  int n;

  n = pic_get_args(pic, "&|o", &self, &val);

  if (n == 0) {
    return var_get(pic, self);
  } else {
    pic_value conv;

    conv = pic_closure_ref(pic, 0);
    if (! pic_false_p(pic, conv)) {
      val = pic_call(pic, conv, 1, val);
    }
    return var_set(pic, self, val);
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
pic_var_with_parameter(pic_state *pic)
{
  pic_value body, val;

  pic_get_args(pic, "l", &body);

  pic->ptable = pic_cons(pic, pic_make_weak(pic), pic->ptable);

  val = pic_call(pic, body, 0);

  pic->ptable = pic_cdr(pic, pic->ptable);

  return val;
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "with-parameter", pic_var_with_parameter);
}
