/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
var_lookup(pic_state *pic, struct pic_proc *var)
{
  pic_value val, env, binding;
  pic_value key = pic_obj_value(var);

  val = pic_ref(pic, pic->PICRIN_BASE, "current-dynamic-environment");
  if (pic_eq_p(val, key)) {
    return pic_false_value();
  }

  env = pic_apply0(pic, pic_proc_ptr(val));
  while (! pic_nil_p(env)) {
    binding = pic_car(pic, env);

    while (! pic_nil_p(binding)) {
      if (pic_eq_p(pic_caar(pic, binding), key)) {
        return pic_car(pic, binding);
      }
      binding = pic_cdr(pic, binding);
    }
    env = pic_cdr(pic, env);
  }

  return pic_false_value();
}

static pic_value
var_call(pic_state *pic)
{
  struct pic_proc *self = pic_get_proc(pic);
  pic_value val, tmp, box, conv;
  int n;

  n = pic_get_args(pic, "|oo", &val, &tmp);

  box = var_lookup(pic, self);
  if (! pic_test(box)) {
    box = pic_proc_env_ref(pic, self, "box");
  }

  switch (n) {
  case 0:
    return pic_cdr(pic, box);

  case 1:
    conv = pic_proc_env_ref(pic, self, "conv");
    if (pic_test(conv)) {
      pic_assert_type(pic, conv, proc);

      val = pic_apply1(pic, pic_proc_ptr(conv), val);
    }
    pic_set_cdr(pic, box, val);

    return pic_none_value();

  case 2:
    assert(pic_false_p(tmp));

    conv = pic_proc_env_ref(pic, self, "conv");
    if (pic_test(conv)) {
      pic_assert_type(pic, conv, proc);

      return pic_apply1(pic, pic_proc_ptr(conv), val);
    } else {
      return val;
    }
  }
  PIC_UNREACHABLE();
}

struct pic_proc *
pic_make_var(pic_state *pic, pic_value init, struct pic_proc *conv)
{
  struct pic_proc *var;

  var = pic_make_proc(pic, var_call, "<var-call>");
  pic_proc_env_set(pic, var, "box", pic_cons(pic, pic_false_value(), init));
  pic_proc_env_set(pic, var, "conv", conv ? pic_obj_value(conv) : pic_false_value());

  return var;
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  struct pic_proc *conv = NULL;
  pic_value init;

  pic_get_args(pic, "o|l", &init, &conv);

  return pic_obj_value(pic_make_var(pic, init, conv));
}

void
pic_init_var(pic_state *pic)
{
  pic_define_noexport(pic, "current-dynamic-environment", pic_false_value());

  pic_defun(pic, "make-parameter", pic_var_make_parameter);

  pic_set(pic, pic->PICRIN_BASE, "current-dynamic-environment", pic_obj_value(pic_make_var(pic, pic_nil_value(), NULL)));
}
